import tc.Gramatica
import util.lecturaTXT
import tc.GramaticaChomsky
import cyk.CYK


object Main {
   def main(args: Array[String]): Unit = {
    println("==================TP: TEORIA DE LA COMPUTACIÓN==================")
    println("==================LECTURA TXT==================")
    val l = new lecturaTXT
    val g = l.leerGramatica()
    println(g)
    
    println("==================LIMPIEZA GRAMÁTICA==================")
    val gl = g.limpiarGramatica()
    println(gl)
    
    println("==================PASAJE A FORMA NORMAL DE CHOMSKY==================")
    val c = new GramaticaChomsky(gl)
    val gc = c.pasaraChomsky()
    println(gc)
    
    println("==================VERIFICAR PERTENENCIA STRING==================")
    print("INGRESE UN STRING: ")
    val s=scala.io.StdIn.readLine()
    val cyk = new CYK(gc)
    if(cyk.analizarPertenenciaString(s)) println("El string: "+s+" pertenece a la gramática")
    else println("El string: "+s+" no pertenece a la gramática")
    
  }
}