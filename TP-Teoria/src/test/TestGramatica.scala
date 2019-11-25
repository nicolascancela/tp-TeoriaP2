package test
import org.scalactic.source.Position.apply
import tc.Gramatica
import tc.Produccion

class Test extends abstractTest {

  "Hay dos producciones alcanzables:" must "S y A" in {
    val v = Set("S", "A", "B")
    val t = Set('a')
    val p1 = Produccion("S", "a")
    val p2 = Produccion("S", "A")
    val g = Gramatica(v, t, p1 :: p2 :: Nil)
    assert(g.obtenerAlcanzables() == Set("S","A"))
  }

  "La variable nulleable es" must "S" in {
    val v = Set("S")
    val t = Set('ε')
    val p1 = Produccion("S", "ε")
    val g = Gramatica(v, t, p1 :: Nil)
    assert(g.obtenerVariablesNulleables() == Set("S"))
  }

  "La produccion S->AB, con A y B nulleable" must "generar producciones S->AB, S->A y S->B" in {
    val v = Set("S", "A", "B")
    val t = Set('a')
    val p1 = Produccion("S", "AB")
    val p2 = Produccion("A", "ε")
    val p3 = Produccion("B", "ε")
    val g = Gramatica(v, t, p1 :: p2 :: p3 :: Nil)
    assert(g.procesarNulleables() == Set(Produccion("S","AB"),Produccion("S","A"), Produccion("S","B")))
  }
  
    "Las variables generadoras" must "son 2: A y B" in {
    val v = Set("S", "A", "B")
    val t = Set('t')
    val p1 = Produccion("S", "X")
    val p2 = Produccion("A", "t")
    val p3 = Produccion("B", "t")
    val g = Gramatica(v, t, p1 :: p2 :: p3 :: Nil)
    assert(g.obtenerVariablesGeneradoras()==Set("A","B"))
  }
    
    
   "La única variable alcanzable" must "es S" in {
    val v = Set("S", "A", "B")
    val t = Set('a','t')
    val p1 = Produccion("S", "a")
    val p2 = Produccion("A", "t")
    val p3 = Produccion("B", "t")
    val g = Gramatica(v, t, p1 :: p2 :: p3 :: Nil)
    assert(g.obtenerAlcanzables()== Set("S"))
  }
   
   "Las producciones unitarias son" must "A->B y S->A" in {
    val v = Set("S", "A", "B")
    val t = Set('t')
    val p1 = Produccion("S", "A")
    val p2 = Produccion("A", "B")
    val p3 = Produccion("B", "t")
    val g = Gramatica(v, t, p1 :: p2 :: p3 :: Nil)
    assert(g.obtenerProduccionesUnitarias().toSet == Set(Produccion("A","B"), Produccion("S","A")))
  }
   
   "La varibale nulleable es" must "A" in {
    val v = Set("S", "A", "B")
    val t = Set('t')
    val p1 = Produccion("S", "AB")
    val p2 = Produccion("A", "ε")
    val p3 = Produccion("B", "t")
    val g = Gramatica(v, t, p1 :: p2 :: p3 :: Nil)
    assert(g.obtenerVariablesNulleables() == Set("A"))
  }
  

}