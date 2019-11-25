package test
import org.scalactic.source.Position.apply
import tc.Gramatica
import tc.Produccion
import cyk.CYK

class TestCYK extends abstractTest {

  "El string ab pertenece a" must "S->AB S->a S->b" in {
    val v = Set("S", "A", "B")
    val t = Set('a','b')
    val p1 = Produccion("S", "AB")
    val p2 = Produccion("A", "a")
    val p3 = Produccion("B", "b")
    val g = Gramatica(v, t, p1 :: p2 :: p3 :: Nil)
    val cyk = CYK(g)
    assert(cyk.analizarPertenenciaString("ab")==true)
  }
  
  
    "El string aq NO pertenece a" must "S->AB S->a S->b" in {
    val v = Set("S", "A", "B")
    val t = Set('a','b')
    val p1 = Produccion("S", "AB")
    val p2 = Produccion("A", "a")
    val p3 = Produccion("B", "b")
    val g = Gramatica(v, t, p1 :: p2 :: p3 :: Nil)
    val cyk = CYK(g)
    assert(cyk.analizarPertenenciaString("aq")==false)
  }
    
    
   "El string acd pertenece acd" must "S->AB A->a B->CD C->c D->d" in {
    val v = Set("S", "A", "B", "C", "D")
    val t = Set('a','b','c','d')
    val p1 = Produccion("S", "AB")
    val p2 = Produccion("A", "a")
    val p3 = Produccion("B", "CD")
    val p4 = Produccion("C", "c")
    val p5 = Produccion("D", "d")
    val g = Gramatica(v, t, p1 :: p2 :: p3 :: p4 :: p5 :: Nil)
    val cyk = CYK(g)
    assert(cyk.analizarPertenenciaString("acd")==true)
  }


}