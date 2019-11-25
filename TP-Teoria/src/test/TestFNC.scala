package test
import org.scalactic.source.Position.apply
import tc.Gramatica
import tc.Produccion
import cyk.CYK
import tc.GramaticaChomsky

class TestFNC extends abstractTest {

  "La producción S->ABC" must "partirse en dos producciones: S->APi y Pi>BC" in {
    val v = Set("S", "A", "B", "C")
    val t = Set('a')
    val p1 = Produccion("S", "ABC")
    val g = Gramatica(v, t, p1 :: Nil)
    val gc = GramaticaChomsky(g)
    assert(gc.pasaraChomsky().producciones.size==2)
  }
  
  "La producción S->At" must "partirse en dos producciones: S->ATi y Ti>t" in {
    val v = Set("S", "A")
    val t = Set('t')
    val p1 = Produccion("S", "At")
    val g = Gramatica(v, t, p1 :: Nil)
    val gc = GramaticaChomsky(g)
    assert(gc.pasaraChomsky().producciones.size==2)
  }
  
    "Para cada terminal a, b, c" must "debo crear una producción Ti>a/b/c" in {
    val v = Set("S", "A")
    val t = Set('a','b','c')
    val p1 = Produccion("S", "At")
    val g = Gramatica(v, t, p1 :: Nil)
    val gc = GramaticaChomsky(g)
    assert(gc.armarTerminales().size==3)
  }
  


}