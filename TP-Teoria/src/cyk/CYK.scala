package cyk
import tc.Gramatica
import tc.Produccion

case class CYK(g: Gramatica) {
  val terminales = g.terminales
  val producciones = g.producciones
  //Vamos a usar un map ==> Map[(Int,Int),List[String]]

  def analizarPertenenciaString(s: String): Boolean = {
    val mapaTerminales = verTerminalesString(s) //Mapeo variables de terminales.
    if (llegoAtodasLasTerminales(mapaTerminales)) { //Si eventualmente, no encuentro un terminal del String, no sigo procesando...
      val mapaAux = (recorrerMapa(mapaTerminales, s))
      stringPertenece(s, mapaAux)
    } else false
  }

  def verTerminalesString(s: String): Map[(Int, Int), List[String]] = {

    //Obtengo las variables que llegan a los terminales, los ubico en un mapa, con posición ij con i=j.
    def recorrerString(s: String, i: Int, j: Int): Map[(Int, Int), List[String]] = {
      if (s.isEmpty()) {
        Map()
      } else {
        val listaAux = producciones.filter(_.produccion.contains(s.head)).map(_.nombreVariable.replaceAll("\\s", ""))
        val aux = Map((i, j) -> listaAux) //Me armo el mapa con la lista de apariciones.
        recorrerString(s.tail, i + 1, j + 1) ++ aux
      }
    }
    recorrerString(s, 1, 1)
  }

  def analizarDiagonales(i: Int, j: Int, mapa: Map[(Int, Int), List[String]]): List[String] = {

    def recorrerMapaDiagonales(i: Int, j: Int, k: Int, mapa: Map[(Int, Int), List[String]]): List[String] = {
      if (k >= j) {
        Nil
      } else {
        println("============================================")
        println("Analizando indices: " + i, j)
        println("El mapa actual es: " + mapa)
        val p1 = mapa.getOrElse((i, k), Nil)
        println(i, k + " da como resultado: " + p1)
        val p2 = mapa.getOrElse(((k + 1), j), Nil)
        println((k + 1), j + " da como resultado: " + p2)
        val producto = productoCartesiano(p1, p2)
        val variablesProducto = obtenerProduccionesVinculadasAProductoCartesiano(producto)
        return recorrerMapaDiagonales(i, j, k + 1, mapa) ::: variablesProducto
      }
    }
    return recorrerMapaDiagonales(i, j, i, mapa)
  }

  def recorrerMapa(mapa: Map[(Int, Int), List[String]], s: String): Map[(Int, Int), List[String]] = {

    def recorrerMapaHorizontal(mapa: Map[(Int, Int), List[String]], i: Int, j: Int, max: Int): Map[(Int, Int), List[String]] = {
      if (j > max) {
        Map()
      } else {
        val p1 = analizarDiagonales(i, j, mapa)
        val mapaAux = Map((i, j) -> p1)
        recorrerMapaHorizontal((mapa ++ mapaAux), i + 1, j + 1, max) ++ mapaAux
      }
    }

    def recorrerMapaVertical(mapa: Map[(Int, Int), List[String]], i: Int, j: Int, max: Int): Map[(Int, Int), List[String]] = {
      if (j > max) {
        return Map()
      } else {
        val auxMapa = recorrerMapaHorizontal(mapa, i, j, max)
        recorrerMapaVertical(mapa ++ auxMapa, 1, j + 1, max) ++ auxMapa
      }
    }
    return recorrerMapaVertical(mapa, 1, 2, s.size) ++ mapa
  }

  def obtenerProduccionesVinculadasAProductoCartesiano(listaProducto: List[String]): List[String] = {
    if (listaProducto.isEmpty)
      "" :: Nil
    else
      obtenerProduccionesVinculadasAProductoCartesiano(listaProducto.tail) ::: matcheoLista(listaProducto.head) ::: Nil
  }

  //Recorro la lista del producto cartesiano, y me fijo si en las producciones encuentro alguna variables que llegue al producto.
  def matcheoLista(producto: String): List[String] = {

    def recorrerProduccionesMatching(producto: String, producciones: List[Produccion]): List[String] = {
      if (producciones.isEmpty)
        Nil
      else {
        //Para comparar si el producto cartesiano es igual a las producciones existentes, primero limpio ambos.
        val p1 = producciones.head.produccion.replaceAll("\\s", "")
        val p2 = producto.replaceAll("\\s", "")
        if (p1 == p2) {
          return recorrerProduccionesMatching(producto, producciones.tail) ::: producciones.head.nombreVariable :: Nil
        }
        return recorrerProduccionesMatching(producto, producciones.tail) ::: Nil
      }
    }
    return recorrerProduccionesMatching(producto, producciones)
  }

  def llegoAtodasLasTerminales(mapa: Map[(Int, Int), List[String]]): Boolean = !mapa.values.exists(p => p.isEmpty)

  //Devuelve las variables que estan vinculadas a un terminal, en caso de encontrarse en varios lados.
  def obtenerProduccionesVinculadasATerminales(lista: List[Produccion]): List[String] = {
    if (lista.isEmpty)
      "" :: Nil //Si no hay producciones vinculadas al terminal i, devuelvo un vacio.
    else
      obtenerProduccionesVinculadasATerminales(lista.tail) ::: lista.head.produccion :: Nil
  }

  //Calcula el producto cartesiano entre dos listas.
  def productoCartesiano(a: List[String], b: List[String]): List[String] = a.flatMap(x => b.map(y => (x + y))).filter(!_.isEmpty())

  def stringPertenece(string: String, mapa: Map[(Int, Int), List[String]]): Boolean = {
    println("Mapa final generado: " + mapa)
    val aux = string.size
    mapa.getOrElse((1, aux), Nil).contains("S")
  }
}