package util
import scala.io.Source
import tc.Produccion
import tc.Gramatica

class lecturaTXT(nombreArchivo: String) {
  
  def leerGramatica(): Gramatica = {
      val txt = leerTXT()
      val variables: Set[String] = procesarVariables(txt)
      val terminales: Set[Char] = procesarTerminales(txt)
      val producciones: List[Produccion] = procesarProducciones(txt)
      new Gramatica(variables, terminales, producciones)
    }

    def leerTXT(): List[String] = {
      val path = "resources/" + nombreArchivo
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