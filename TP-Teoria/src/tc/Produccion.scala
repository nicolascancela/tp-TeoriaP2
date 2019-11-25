package tc

case class Produccion(nombreVariable: String, produccion: String) {
  
  def produccionVacia() = produccion.isEmpty
  
  def eliminarEpsilon(): Unit = produccion.replace("ε","")
  def tieneTerminales(): Boolean = {

    //@annotation.tailrec: CONSULTAR!
    def tieneunTerminal(s: String): Boolean = {
      if (s.isEmpty()) {
        return false
      } else {
        return tieneunTerminal(s.tail) || s.head.isLower
      }
    }
    tieneunTerminal(produccion)
  }
  
   def esNulleable(nulleados: Set[String]): Boolean = {
  
    def esNulleablelaProduccion(p:String, nulleados: Set[String]): Boolean = {
        if(p.isEmpty()){
          return true
        }
        else{
          val estaEnNulleados = nulleados.contains(""+p.head)
          return esNulleablelaProduccion(p.tail,nulleados) && estaEnNulleados
        }  
        
    }
        return esNulleablelaProduccion(produccion,nulleados)
  }
   
   
  def esGenerador(generadores: Set[String]): Boolean = {
  
    def esGeneradoralaProduccion(p:String, generadores: Set[String]): Boolean = {
        if(p.isEmpty()){
          return false
        }
        else{
          val estaEnGeneradores = generadores.contains(""+p.head)
          return esGeneradoralaProduccion(p.tail,generadores) || estaEnGeneradores
        }  
        
    }
        return esGeneradoralaProduccion(produccion,generadores)
  }
  
  def obtenerTerminalesProduccion(): Set[Char] = {
    
    def getTerminales(p: String): Set[Char] = {
      if(p.isEmpty()){
        return Set()
      }
      else{
        if(p.head.isLower){
          return getTerminales(p.tail) + p.head
        }
          return getTerminales(p.tail)
      }
    }
    return getTerminales(produccion)
  }
    
  def obtenerVariablesProduccion(): Set[String] = {
    
    def obtenerVariables(p: String): Set[String] = {
      if(p.isEmpty()){
        return Set()
      }
      else{
        if(p.head.isUpper){
          return obtenerVariables(p.tail) + (""+p.head)
        }
        return obtenerVariables(p.tail)
      }
    }
    
    return obtenerVariables(produccion)
  }
  
  def esVacia(): Boolean = produccion.isEmpty()

  def esEpsilon(): Boolean = produccion=="ε"
  
  def esUnitaria(): Boolean = produccion.size==1 && produccion.head.isUpper
  
  def contieneTerminal(): Boolean = {
     
    def esLower(p: String): Boolean = {
      if(p.isEmpty()){
        return false
      }
    return esLower(p.tail) || p.head.isLower
    }
    return esLower(produccion.replaceAll("\\s", ""))
  }
  
  def esTerminal(): Boolean = produccion.equals(produccion.toLowerCase()) || !produccion.equals('ε')
  
  def esTerminalNuevo(): Boolean = produccion.size==1 && produccion.head.isLower 
  

  override def toString() : String = { 
        return nombreVariable + "->"+produccion
    } 

  //CumpleChomsky: Si tiene un terminal, o dos variables...
  def cumpleChomsky(): Boolean = (produccion.size==1 && produccion.head.isLower) || (produccion.size==2 && produccion== produccion.toUpperCase())
}