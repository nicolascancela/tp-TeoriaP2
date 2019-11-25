package tc
import scala.io.Source

case class Gramatica(variables: Set[String], terminales: Set[Char], producciones: List[Produccion]) {
  val inicial = "S" //Luego se lo utilizará para obtener los alcanzables.

  def filtrarProduccionesNoUnitarias(): List[Produccion] = producciones.filter(_.esUnitaria() == false)
  
  def filtrarProduccionesUnitarias(): List[Produccion] = producciones.filter(_.esUnitaria())
  
  def buscarProduccionesdeVariables(variable: String): List[Produccion] = producciones.filter(_.nombreVariable == variable)
  
  def buscarProducciones(variable: String): List[Produccion] = producciones.filter(_.produccion.contains(variable))
  
  def buscarProduccionesEpsilon(): List[Produccion] = producciones.filter(_.produccion.equals("ε"))
 
  def filtrarProduccionesVariables(v: String): List[Produccion] = producciones.filter(_.nombreVariable == v)
  
  def limpiarGramatica(): Gramatica = {
    val produccionesSinNulleables = procesarNulleables()
    val produccionesNoUnitarias = procesarProduccionesUnitarias().filter(!_.esEpsilon()) ++ producciones.filter(!_.esUnitaria())
    val variablesGeneradoras = obtenerVariablesGeneradoras()
    val produccionesGeneradoras: Set[Produccion] = producciones.filter(p => variablesGeneradoras.contains(p.nombreVariable)).toSet
    val variablesNoAlcanzables = obtenerNoAlcanzables()
    val produccionesNoAlcanzables: Set[Produccion] = producciones.filter(p => variablesNoAlcanzables.contains(p.nombreVariable)).toSet
    val nuevasProducciones = (produccionesSinNulleables ++ produccionesNoUnitarias ++ produccionesGeneradoras -- produccionesNoAlcanzables).filter(p => p.esEpsilon() == false).filter(!_.esUnitaria())
    val produccionesFinal = eliminarSimboloEpsilonProducciones(nuevasProducciones)
    val nuevasVariables: Set[String] = produccionesFinal.map(_.nombreVariable)
    val nuevosTerminales: Set[Char] = produccionesFinal.flatMap(_.obtenerTerminalesProduccion())
    new Gramatica(nuevasVariables, nuevosTerminales, produccionesFinal.toList)
  }

  def eliminarSimboloEpsilonProducciones(p: Set[Produccion]): Set[Produccion] = {
    if(p.isEmpty){
      Set()
    }
    else{
      val actual = p.head
      val nuevaProduccion = Produccion(actual.nombreVariable, actual.produccion.replaceAll("ε", ""))
      eliminarSimboloEpsilonProducciones(p.tail) + nuevaProduccion
    }
  }
  //RECIBE UNA LISTA DE PRODUCCIONES Y DEVUELVE LAS VARIABLES DE LAS PRODUCCIONES.
  def obtenerVariablesProduccion(p: List[Produccion]): Set[String] = {
    if (p.isEmpty) {
      Set()
    } else {
      obtenerVariablesProduccion(p.tail) ++ p.head.obtenerVariablesProduccion() + p.head.nombreVariable
    }
  }

  //RECIBE UNA VARIABLE Y UN SET DE STRING Y GENERA PRODUCCIONES ASOCIADAS A LA VARIABLE.
  def armarProducciones(variable: String, produccionesNuevas: Set[String]): List[Produccion] = {
    if (produccionesNuevas.isEmpty) {
      Nil
    } else {
      val nuevaProduccion = Produccion(variable, produccionesNuevas.head)
      armarProducciones(variable, produccionesNuevas.tail) ::: nuevaProduccion :: Nil
    }
  }

  //Procesa las producciones nulleables, genera las combinaciones y devuelve un nuevo conjunto de producciones.
  def procesarNulleables(): Set[Produccion] = {

    def generarCombinaciones(variableNula: String, produccion: String, prefijo: String): Set[String] = {
      if (produccion.isEmpty()) {
        return Set()
      } else {
        val palabra = (prefijo + produccion.head + produccion.tail).replaceAll("ε", "")
        if (("" + produccion.head) == variableNula) {
          val palabraSinV = (prefijo + produccion.tail).replaceAll("ε", "")
          return generarCombinaciones(variableNula, produccion.tail, prefijo + produccion.head) ++
            generarCombinaciones(variableNula, produccion.tail, prefijo) + palabraSinV + palabra
        }
        return generarCombinaciones(variableNula, produccion.tail, prefijo + produccion.head) + palabra
      }
    }

    def aplicarGenerarCombinaciones(variableNula: String, combinaciones: Set[String]): Set[String] = {
      if (combinaciones.isEmpty) {
        Set()
      } else {
        aplicarGenerarCombinaciones(variableNula, combinaciones.tail) ++ generarCombinaciones(variableNula, combinaciones.head, "")
      }
    }

    def funcion(variablesNulas: Set[String], prodActual: Produccion, combinaciones: Set[String]): Set[String] = {
      if (variablesNulas.isEmpty) {
        return Set()
      } else {
        val combinacionesNuevas = generarCombinaciones(variablesNulas.head, prodActual.produccion, "") ++ aplicarGenerarCombinaciones(variablesNulas.head, combinaciones)
        return funcion(variablesNulas.tail, prodActual, combinaciones ++ combinacionesNuevas) ++ combinaciones ++ combinacionesNuevas
      }
    }

    def armarProduccionesNulleables(variablesNulas: Set[String], prod: Set[Produccion]): Set[Produccion] = {
      if (prod.isEmpty) {
        Set()
      } else {
        val p = prod.head
        armarProduccionesNulleables(variablesNulas, prod.tail) ++ armarProducciones(p.nombreVariable, funcion(variablesNulas, p, Set())).toSet
      }
    }

    val prodNoEpsilon = producciones.filter(!_.esEpsilon())
    return armarProduccionesNulleables(obtenerVariablesNulleables(), prodNoEpsilon.toSet).filter(!_.esVacia())
  }

  def obtenerProduccionesUnitarias(): List[Produccion] = return producciones.filter(_.esUnitaria())

  //Devuelve un conjunto de producciones YA no unitarias.
  def procesarProduccionesUnitarias(): Set[Produccion] = {

    def armarNuevasProducciones(variable: String, lista: List[Produccion]): Set[Produccion] = {
      if (lista.isEmpty)
        Set()
      else
        armarNuevasProducciones(variable, lista.tail) + Produccion(variable, lista.head.produccion)
    }

    def procesarProducciones(existentes: List[Produccion], nuevas: Set[Produccion]): Set[Produccion] = {
      if (existentes.isEmpty) {
        return Set()
      } else {
        val prodActual = existentes.head
        val listaProduccionesNoUnitarias = producciones.filter(_.nombreVariable == prodActual.produccion) //Filtro las producciones NO unitarias para armar nuevas producciones
        val nuevasProducciones = (armarNuevasProducciones(prodActual.nombreVariable, listaProduccionesNoUnitarias))
        val nuevasProduccionesUnitarias = nuevasProducciones.filter(_.esUnitaria())
        return procesarProducciones((existentes ++ nuevasProduccionesUnitarias).tail, (nuevas ++ nuevasProducciones)) ++ nuevasProducciones
      }
    }
    return procesarProducciones(obtenerProduccionesUnitarias(), Set())
  }

  //Devuelve un conjunto de variables generadoras.
  def obtenerVariablesGeneradoras(): Set[String] = {

    def obtenerGeneradoras(lista: List[Produccion], generadores: Set[String]): Set[String] = {
      if (lista.isEmpty) {
        return Set()
      } else {
        val nuevosGeneradores = lista.filter(_.esGenerador(generadores)) //Filtro todas las producciones que tengan generadoras X =>* G donde G esta en el conj generadores.
        val variablesGeneradoras = nuevosGeneradores.map(_.nombreVariable) //Obtengo las nuevas variables generadoras
        val prodAux = producciones.filter(p => !generadores.contains(p.nombreVariable)) //Filtro aquellas producciones que aún contengan generadoras, pero que no hayan sido descubiertas aun.
        if (nuevosGeneradores.isEmpty) { //Si ya no encuentro más generadores, mando una lista vacia.
          return obtenerGeneradoras(Nil, (generadores ++ variablesGeneradoras)) ++ generadores
        }
        return obtenerGeneradoras(prodAux, (generadores ++ variablesGeneradoras)) ++ generadores
      }
    }
    val aux = producciones.filter(_.esTerminalNuevo()) //val aux = producciones.filter(_.contieneTerminal())
    val generadorasInicial = aux.map(_.nombreVariable).toSet
    return obtenerGeneradoras(producciones, generadorasInicial)
  }
  
  
  //Devuelve un conjunto de variables generadoras.
  def obtenerProduccionesGeneradoras(): Set[Produccion] = {

    def obtenerProGeneradoras(prod: Set[Produccion], generadores: Set[String]): Set[Produccion] = {
      if (prod.isEmpty) {
        return Set()
      } else {
        val nuevosGeneradores = prod.filter(_.esGenerador(generadores)) //Filtro todas las producciones que tengan generadoras X =>* G
        val variablesGeneradoras = nuevosGeneradores.map(_.nombreVariable) //Obtengo las nuevas variables generadoras
        val prodAux = producciones.filter(p => !generadores.contains(p.nombreVariable)) //Filtro aquellas producciones que aún contengan generadoras, pero que no hayan sido descubiertas aun.
        if (nuevosGeneradores.isEmpty) { //Si ya no encuentro más generadores, mando una lista vacia.
          return obtenerProGeneradoras(Set(), (generadores ++ variablesGeneradoras)) ++ nuevosGeneradores
        }
        return obtenerProGeneradoras(prodAux.toSet, (generadores ++ variablesGeneradoras)) ++ nuevosGeneradores
      }
    }
    val aux = producciones.filter(_.esTerminalNuevo()) //val aux = producciones.filter(_.contieneTerminal())
    val generadorasInicial = aux.map(_.nombreVariable).toSet
    return obtenerProGeneradoras(producciones.toSet, generadorasInicial) ++ aux
  }  

  //PERFECTO
  def obtenerVariablesNulleables(): Set[String] = {

    def obtenerNulleables(lista: List[Produccion], nulleados: Set[String]): Set[String] = {
      if (lista.isEmpty) {
        return Set()
      } else {
        val nuevosNulleados = lista.filter(_.esNulleable(nulleados)) //Filtro todas las producciones que sean nulleables leyendo el conjunto enviado por parametro.
        val variablesNulleados = obtenerVariablesProduccion(nuevosNulleados) //Obtengo las nuevas variables nulleada
        val prodAux = producciones.filter(p => !nulleados.contains(p.nombreVariable)) //Filtro aquellas producciones que ya hayan sido nulleables.
        if (nuevosNulleados.isEmpty) { //Si ya no encuentro más nulleables, mando una lista vacia.
          return obtenerNulleables(Nil, (nulleados ++ variablesNulleados)) ++ nulleados
        }
        return obtenerNulleables(prodAux, (nulleados ++ variablesNulleados)) ++ nulleados
      }
    }
    val produccionesEpsilon = producciones.filter(_.esEpsilon())
    val inicial = obtenerVariablesProduccion(produccionesEpsilon)
    return obtenerNulleables(producciones, inicial) ++ inicial
  }

  //Devuelve un conjunto de variables alcanzables desde una variable inicial S.
  def obtenerAlcanzables(): Set[String] = {

    def alcanzablesdesdeV(v: Set[String], yaAnalizados: Set[String]): Set[String] = {
      if (v.isEmpty) {
        Set()
      } else {
        val variablesProduccion = producciones.filter(_.nombreVariable == v.head)
        val conjuntoAlcanzables = obtenerVariablesProduccion(variablesProduccion).toSet
        val todo = v ++ conjuntoAlcanzables -- yaAnalizados
        alcanzablesdesdeV(todo, yaAnalizados + v.head) ++ obtenerVariablesProduccion(variablesProduccion) ++ v
      }
    }
    alcanzablesdesdeV(Set(inicial), Set())
  }

  def obtenerNoAlcanzables(): Set[String] = variables -- obtenerAlcanzables()

  def hallarVariablesProduccion(variable: String): Set[String] = {

    def filtrarVariablesDerivacion(lista: List[Produccion]): Set[String] = {
      if (lista.isEmpty) {
        Set()
      } else {
        filtrarVariablesDerivacion(lista.tail) ++ lista.head.obtenerVariablesProduccion()
      }
    }
    variables & filtrarVariablesDerivacion(producciones.filter(_.nombreVariable == variable))
  }

  override def toString(): String = {
    "VARIABLES: " + variables + "\nTERMINALES: " + terminales + "\nPRODUCCIONES: " + producciones
  }

  //CONSTRUCTOR CONVENIENCE!
  object Gramatica {

    def apply(): Gramatica = {
      val txt = leerTXT()
      val variables: Set[String] = procesarVariables(txt)
      val terminales: Set[Char] = procesarTerminales(txt)
      val producciones: List[Produccion] = procesarProducciones(txt)
      new Gramatica(variables, terminales, producciones)
    }

    def leerTXT(): List[String] = {
      val path = "resources/G.txt"
      val archivo = Source.fromResource(path)
      val lineas = archivo.getLines().toList
      archivo.close()
      lineas
    }

    def procesarProducciones(lista: List[String]): List[Produccion] = {
      if (lista.isEmpty) {
        Nil
      } else {
        val varAux = lista.head.split("->") //Parto el String en dos!
        val prodAux = Produccion(varAux(0), varAux(1)) //Creo mi producción!
        val producciones = prodAux :: Nil //Armo una sublista
        producciones ::: procesarProducciones(lista.tail) //Lo anexo a la lista!
      }
    }

    def procesarVariables(lista: List[String]): Set[String] = {
      if (lista.isEmpty)
        Set() //Si no hay más lista, devuelvo un conjunto vacio.
      else {
        val varAux = lista.head.split("->") //Parto el String en dos!
        val x = varAux(0) //Creo mi variable!
        Set(x) ++ procesarVariables(lista.tail) //Lo agrego al conjunto!
      }
    }

    def procesarTerminales(lista: List[String]): Set[Char] = {

      def procesarStringTerminales(s: String): Set[Char] = {
        if (s.isEmpty)
          Set() //Si no hay más elementos en la lista, devuelvo un conjunto vacio.
        else
          Set(s.head).filter(_.isLower) ++ procesarStringTerminales(s.tail) //Filtro las variables ;)
      }

      if (lista.isEmpty)
        Set() //Si no hay más elementos en la lista, devuelvo un conjunto vacio.
      else
        procesarStringTerminales(lista.head) ++ procesarTerminales(lista.tail) //sino agrego al string conjunto!
    }
  }
}