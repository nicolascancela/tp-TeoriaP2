package tc

case class GramaticaChomsky(g: Gramatica) {
  val terminales = g.terminales
  val producciones = agregarEspacios(g.producciones.toSet)
  val auxNumeros = 1 to 100 toSet
  val variablesDisponibles = auxNumeros.map(n => (" T" + n))
  val variablesProducciones = auxNumeros.map(n => (" P" + n))

  def pasaraChomsky(): Gramatica = {
    val w = agregarEspacios(producciones.toSet) //Lo usamos para saber que producciones son mayores a 3 componentes.
    val x = armarTerminales()
    val y = procesarGramatica(x,producciones)
    val z = armarProducciones(y).reverse
    val nuevasVariables = z.map(_.nombreVariable).toSet
    val nuevasProducciones = z
    new Gramatica(nuevasVariables, terminales, nuevasProducciones.toList)
  }
  
  def agregarEspacios(prodAux: Set[Produccion]): Set[Produccion] = {
    if(prodAux.isEmpty){
      return Set()}
    else{
      val prodActual = prodAux.head
      val ladoDerecho = prodActual.produccion.flatMap(x => (" " + x))
      val nuevaProd = Produccion(prodActual.nombreVariable, ladoDerecho)
      return agregarEspacios(prodAux.tail) + nuevaProd
    }
  }

  def procesarGramatica(term: Set[Produccion], prod: Set[Produccion]): Set[Produccion] = {

    def modificarProduccion(s: String, prefijo: String, p: Produccion, term: Set[Produccion]): Set[Produccion] = {
      if (s.isEmpty) {
        Set(Produccion(p.nombreVariable, prefijo))
      } else {
        if (s.head.isLower) {
          val aux = term.filter(p => p.produccion == "" + s.head).head
          return modificarProduccion(s.tail, (prefijo + aux.nombreVariable), p, term) + aux
        }
        return modificarProduccion(s.tail, prefijo + s.head, p, term)
      }
    }

    def recorrerProducciones(prod: Set[Produccion], term: Set[Produccion]): Set[Produccion] = {
      if (prod.isEmpty) {
        return Set()
      } else {
        val actual = prod.head
        val aux = actual.produccion.replaceAll("\\s", "")
        if (actual.contieneTerminal && aux.size > 1) {
          return recorrerProducciones(prod.tail, term) ++ modificarProduccion(actual.produccion, "", actual, term)
        }
        return recorrerProducciones(prod.tail, term) + actual
      }
    }
    return recorrerProducciones(prod, term)
  }

  def armarTerminales(): Set[Produccion] = {
    def procesarTerminales(t: Set[Char], variablesUsadas: Set[String]): Set[Produccion] = {
      if (t.isEmpty) {
        Set()
      } else {
        val nuevaVariable = (variablesDisponibles -- variablesUsadas).head //Agarro de la bolsa de letras disponibles la primera que encuentre
        val nuevaProduccion = Produccion(nuevaVariable, "" + t.head)
        return procesarTerminales(t.tail, variablesUsadas + nuevaVariable) + nuevaProduccion
      }
    }
    return procesarTerminales(terminales, Set())
  }

  def revincularVariables(nuevasProducciones: Set[Produccion]): Set[Produccion] = {

    def vincularTerminales(prod: String, variable: String, prefijo: String): Set[Produccion] = {
      if (prod.isEmpty()) {
        return Set(Produccion(variable, prefijo))
      } else {
        val prodActual = prod.head
        val prodTerminal = nuevasProducciones.filter(_.produccion == ("" + prodActual))
        if (!prodTerminal.isEmpty) {
          //Le agregue un espacio para hacer un split despues.
          val nuevaProduccion = Produccion(variable, prodTerminal.head.nombreVariable) //Me quedo con la unica produccion de la terminal.
          val aux = prefijo + prodTerminal.head.nombreVariable
          return vincularTerminales(prod.tail, variable, aux)
        }
        //Le agrego un espacio para separar las variables
        return vincularTerminales(prod.tail, variable, prefijo + " " + prod.head)
      }
    }

    def vincularTerminalesAnuevasProducciones(prodViejas: Set[Produccion]): Set[Produccion] = {
      if (prodViejas.isEmpty) {
        Set()
      } else {
        val aux = prodViejas.head
        return vincularTerminalesAnuevasProducciones(prodViejas.tail) ++ vincularTerminales(aux.produccion, aux.nombreVariable, "")
      }
    }
    return vincularTerminalesAnuevasProducciones(producciones.toSet)
  }

  def armarProducciones(nuevasProducciones: Set[Produccion]): List[Produccion] = {

    def armarParesdeProducciones(variable: String, listaProduccion: List[String], variablesUsadas: Set[String], prefijo: String): List[Produccion] = {
      if (listaProduccion.isEmpty) {
        return Nil
      }
      val izq = listaProduccion.head
      val nuevaVariable = (variablesProducciones -- variablesUsadas).head
      if(listaProduccion.size==2){ //CASO: S->A B C: S-> A P3, P3-> B C
        val p1 = Produccion(variable, izq +" "+ listaProduccion.tail.head)
        return armarParesdeProducciones(nuevaVariable, Nil, variablesUsadas + nuevaVariable, prefijo + nuevaVariable) ::: (p1 :: Nil)
      }
      
      if (listaProduccion.tail.isEmpty) { //Si llego a la ultima produccion, no hago un par de variables, solo pongo una.
        val p1 = Produccion(variable, izq)
        return armarParesdeProducciones(nuevaVariable, listaProduccion.tail, variablesUsadas + nuevaVariable, prefijo + nuevaVariable) ::: (p1 :: Nil)
      }
      val p1 = Produccion(variable, izq + " " + nuevaVariable)
      return armarParesdeProducciones(nuevaVariable, listaProduccion.tail, variablesUsadas + nuevaVariable, prefijo + nuevaVariable) ::: (p1 :: Nil)
    }

    def recorrerProducciones(p: Set[Produccion]): List[Produccion] = {
      if (p.isEmpty) {
        Nil
      } else {
        val prodActual = p.head
        //Problema: Ahora las producciones se llaman T12, va a pensar que son varias! vamos a tener que separarlas!
        val prod = prodActual.produccion.replaceAll(" +", " ") //Normalizo aquellas producciones con espacios de más!
        val varProd = prodActual.nombreVariable
        val aux = prod.split(" ").map(_.replaceAll("\\s", "")).filter(p => p != "").toList
        //SOLUCIÓN: Pongo un espacio entre los strings de las producciones y pregunto si tienen más de 2 producciones
        if (aux.length > 2) { //Si tengo más de 3 variables en una produccion, la desarmo.
          return recorrerProducciones(p.tail) ++ armarParesdeProducciones(varProd, aux, Set(), "")
        } else {
          return recorrerProducciones(p.tail) :+ p.head //Si mi produccion es menor a 2 producciones, no modifico las producciones.
        }
      }
    }
    return recorrerProducciones(nuevasProducciones)
  }

}