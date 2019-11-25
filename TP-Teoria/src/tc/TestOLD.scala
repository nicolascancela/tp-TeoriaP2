package tc

import scala.io.Source
import cyk.CYK

object TestOLD {

  def main(args: Array[String]): Unit = {
    val z = Produccion("S", "ABCCDX")
    val con = Set("E","X","A","C")
    val u = Produccion("A","Pz")
    println("Z ES GENERADOR" + z.esGenerador(con))
    println("U ES GENERADOR" + u.esGenerador(con))
    val li = u :: z :: Nil
    println("GENERATOOORES")
    val a = li.filter(_.esGenerador(con))
    println("Resultado"+ a.size)
    //LEO EL TXT
    val path = "resources/G.txt"
    val archivo = Source.fromResource(path)
    val lineas = archivo.getLines().toList
    archivo.close()

    //CREO LOS DATOS DE LA GRAMATICA
    val variables = procesarVariables(lineas)
    val terminales = procesarTerminales(lineas)
    val producciones = procesarProducciones(lineas)
    val p1 = Produccion("S","a")
    val p2 = Produccion("S","Qa")
    println(p1.contieneTerminal() && p1.produccion.size>1)
    println(p2.contieneTerminal() && p2.produccion.size>1)
    
    /*
    val v = Set("S", "A", "B")
    val t = Set('a')
    val p5 = Produccion("S", "a")
    val p7 = Produccion("S", "A B C")
    val g = Gramatica(v,t,p5::p7::Nil)
    val h = GramaticaChomsky(g).pasaraChomsky()
    println(h)
    */
    //CREO LA GRAMATICA
   // val g = Gramatica(variables, terminales, producciones).limpiarGramatica()
   //println(g)
    //println(g)
    //println(g.limpiarGramatica())
    //val g = Gramatica(Set(),Set(),Nil)
    
    //val h = GramaticaChomsky(g.limpiarGramatica()).pasaraChomsky()
   //println("GRAMATICA CHOMSKY")
   
    //val i = CYK(h)
    //println(i.analizarPertenenciaString("baab"))
    //println("EJEMPLO DIAPO")
    //val j = CYK(g)
    //println(j.analizarPertenenciaString("baab"))
    //val j = CYK(g)
   // println(j.analizarPertenenciaString("bba"))
    
    //println(h.pasaraChomsky())
    
    //println(g.obtenerVariablesGeneradoras())

    /*
    for (t <- terminales) {
     println("TERMINALES: "+t)}


     for (d <- variables) {
     println("VARIABLES: "+d)}

    for (c <- producciones) {
     println("PRODUCCIÓN: "+c)
     println("TEST TIENE TERMINALES: "+c.tieneTerminales())
     println("TEST ESTA VACIA: "+ c.esVacia())
     println("TEST ES UNITARIAS: "+c.esUnitaria())
     println("CUMPLE CHOMSKY: "+c.cumpleChomsky())
     println("TIENE EPSILON?: "+c.tieneEpsilon())
      }*/
    
    //val auxNumeros = 1 to 10 toSet
    //val auxLetras : Set[String] = Set("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","X","Y","Z")
    //val auxLetras = ('A' to 'Z').map(f => (""+f))
    //val todasVariables = auxNumeros.flatMap(x => auxLetras.map(y => (y+""+x)))
    //print(todasVariables)


    //println(g.procesarProduccionesUnitarias())
    //println(g.obtenerProduccionesUnitarias())
    //println(g.procesarNulleables())
    //val h = Gramatica //Consultar como crear objectos con el constructor!
    //println(g.producciones)
    //println(g.pasaraChomsky())
   // println(g.procesarNulleables())
    //println("VARIABLES NULLEABLES: "+g.obtenerVariablesNulleables())
    //println("PRODUCCIONES -NO- NULLEABLES: "+g.procesarNulleables())
    //println("VARIABLES ALCANZABLES: "+g.obtenerAlcanzables())
    //println("VARIABLES NO ALCANZABLES: "+g.obtenerNoAlcanzables())
    //println("PRODUCCIONES UNITARIAS: "+g.obtenerProduccionesUnitarias())
    //println("PROCESAR UNITARIAS: " + g.procesarProduccionesUnitarias())
    //println(g.obtenerNulleables())
    //println(g.obtenerAlcanzables())
    //println(g.obtenerVariablesNulleables())
    //println(g.obtenerVariablesGeneradoras())
    //println("PRODUCCIONES UNITARIAS: " + g.obtenerProduccionesUnitarias())
    //println("PRODUCCIONES UNITARIAS CON TODO: "+g.procesarProduccionesUnitarias())
    //println("ALCANZABLES: "+ g.obtenerAlcanzables())
    //println(g.filtrarProduccionesEpsilon())
   // println(g.filtrarProduccionesEpsilon())
   // println(g.filtrarProduccionesVariables("S"))
    //println(g.encontrarVariablesNulleables())
    //println(g.obtenerAlcanzables())
    //println(g.noAlcanzables())
    //println(g.obtenerVariablesGeneradoras())
    //println(g.nulleablesNEW())
    
    //val aux = g.producciones.filter(_.esEpsilon())
    //println(aux)
    //val test = g.obtenerVariablesProduccion(aux)
    //println(test)
    //print(producciones.filter(_.esNulleable(test)))

    //val lista: List[String] = "Hola" :: "Chau" :: "Gato" :: Nil
    val lista: List[String] = "MMNN" :: Nil
    
    //val aux = (eliminarX("M", lista)).toList
    
    //print(eliminarX("N",aux))
    //println(g.filtrarProduccionesNoUnitarias())
    //println(g.filtrarProduccionesUnitarias())
    //println(g.buscarProduccionesdeVariables("S"))
    //println(g.buscarProduccionesNulleables())
    //println(g.buscarProducciones("B"))
  }
  
      
  
  def generarCombinaciones(variableNula: String, produccion: String, prefijo: String): Set[String] = {
      if (produccion.isEmpty()) {
        return Set()
      } else {
        val palabra = prefijo + produccion.head + produccion.tail
        if (("" + produccion.head) == variableNula)
        {
          val palabraSinV = prefijo + produccion.tail
          return generarCombinaciones(variableNula, produccion.tail, prefijo + produccion.head) ++ 
                 generarCombinaciones(variableNula, produccion.tail, prefijo) + palabraSinV + palabra
        }
         return  generarCombinaciones(variableNula, produccion.tail, prefijo + produccion.head) + palabra
      }
    }
  
   def eliminarX(x: String, lista: List[String]): Set[String] = {
     
     //PROBLEMA: PUEDO TENER VARIAS VARIABLES NULAS AL MISMO TIEMPO: CASO S->XXYY X->EPSILON Y->EPSILON ==> no se genera XY
     def generarCombinaciones(variableNula: String, produccion: String, prefijo: String): Set[String] = {
      if(produccion.isEmpty()){
        return Set()
      }
      else{
        val palabra = prefijo + produccion.head + produccion.tail
        println("ARMADITA: "+palabra)
        if((""+produccion.head) == variableNula){
          val palabraSinV = prefijo + produccion.tail
          println("SIN V: " + palabraSinV)
          return generarCombinaciones(variableNula, produccion.tail, prefijo + produccion.head) ++ generarCombinaciones(variableNula, produccion.tail,prefijo) + palabraSinV
        }
        return generarCombinaciones(variableNula,produccion.tail, prefijo + produccion.head) + palabra
      }
    }

    def generarCombinacionesOLD(x: String, p: String, prefijo: String): Set[String] = {
      if (p.isEmpty()) {
        return Set()
      } else {
          //println("HEAD DE P:" + p.head)
          //println("TAIL DE P:" + p.tail)
          //println("PREFIJO: " + prefijo)
        if (p.head == x.head) {
            return generarCombinaciones(x, p.tail, prefijo)+((prefijo + p.tail),(p))
        }
        else{
        return generarCombinaciones(x, p.tail,prefijo+p.head)
      }}
    }

    if (lista.isEmpty) {
      return Set()
    } else {
      return (generarCombinaciones(x, lista.head,"") ++ eliminarX(x, lista.tail))
    }
  }

  /*def eliminarX(x: String, lista: List[String]): List[String] = {

    def generarCombinaciones(x: String, p: String): String = {
      if (p.isEmpty()) {
        return ""
      } else {
        if (p.head == x.head) {
          println(p.head)
          //Si encontré la letra que quiero sacar, llamo al metodo con dos variantes, una con la letra y otra sin la letra.
          //Genero el string sin la x, y luego genero el string con la X!
            return generarCombinaciones(x, p.tail) + (p.head + generarCombinaciones(x, p.tail))
          
        }
        return p.head + generarCombinaciones(x, p.tail)
      }
    }

    if (lista.isEmpty) {
      return Nil
    } else {
      return generarCombinaciones(x, lista.head) :: eliminarX(x, lista.tail)
    }
  }*/

  def procesarVariables(lista: List[String]): Set[String] = {
    if (lista.isEmpty) {
      return Set() //Si no hay más lista, devuelvo un conjunto vacio.
    } else {
      val varAux = lista.head.split("->") //Parto el String en dos!
      val x = varAux(0) //Creo mi variable!
      return Set(x) ++ procesarVariables(lista.tail) //Lo agrego al conjunto!
    }
  }

  def procesarTerminales(lista: List[String]): Set[Char] = {

    def procesarStringTerminales(s: String): Set[Char] = {
      if (s.isEmpty) {
        return Set() //Si no hay más lista, devuelvo un conjunto vacio.
      } else {
        var c = s.head;
        return Set(c).filter(_.isLower) ++ procesarStringTerminales(s.tail) //Filtro las variables ;)
      }
    }

    if (lista.isEmpty) {
      return Set() //Si no hay más lista, devuelvo un conjunto vacio.
    } else {
      val x = procesarStringTerminales(lista.head) //Agrego los conjuntos de terminales
      return x ++ procesarTerminales(lista.tail) //Lo agrego al conjunto final!
    }
  }

  def procesarProducciones(lista: List[String]): List[Produccion] = {
    if (lista.isEmpty) {
      return Nil
    } else {
      val varAux = lista.head.split("->") //Parto el String en dos!
      val prodAux = Produccion(varAux(0), varAux(1)) //Creo mi producción!
      val producciones = prodAux :: Nil //Armo una sublista
      return producciones ::: procesarProducciones(lista.tail) //Lo anexo a la lista!
    }
  }
}