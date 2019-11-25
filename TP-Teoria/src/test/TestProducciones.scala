package test
import org.scalactic.source.Position.apply
import tc.Gramatica
import tc.Produccion
import cyk.CYK

class TestProducciones extends abstractTest {

  "La produccion AbC" must "tiene un terminal" in {
    val p1 = Produccion("S", "AbC")
    assert(p1.tieneTerminales() == true)
  }

  "La produccion G es nulleable" must "A->G entonces A es nulleable." in {
    val nulleables = Set("G")
    val p1 = Produccion("A", "G")
    assert(p1.esNulleable(nulleables) == true)
  }

  "La produccion G es generadora" must "A->G entonces A es generadora." in {
    val generadores = Set("G")
    val p1 = Produccion("A", "G")
    assert(p1.esGenerador(generadores) == true)
  }
  
   "La produccion G->abHc" must "tiene a b c terminales." in {
    val p1 = Produccion("G", "abHc")
    assert(p1.obtenerTerminalesProduccion()==Set('a','b','c'))
  }
   
   
   "La produccion G->abHc" must "tiene 1 variable H." in {
    val p1 = Produccion("G", "abHc")
    assert(p1.obtenerVariablesProduccion()==Set("H"))
  }
   
   "La produccion G->abεc" must "no es Epsilon" in {
    val p1 = Produccion("G", "abεc")
    assert(p1.esEpsilon()==false)
  }

   "La produccion G->a" must "es terminal" in {
    val p1 = Produccion("G", "a")
    assert(p1.esTerminalNuevo()==true)
  }
}