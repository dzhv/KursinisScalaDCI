package scaladci
package examples

/*
  DCI pavyzdys kursiniam darbui
  Džiugas Vyšniauskas
*/

class Transportas extends Specification {
  
  case class TransportoPriemonė(tipas: String, kilometražas: Double, kelionėsKaina: Double) {}
  case class Vairuotojas(vardas: String, pavardė: String, pinigai: Double, stažas: Double) {}
  case class Keleivis(vardas: String, pavardė: String, pinigai: Double, bilietai: Int) {
    def pasižymėtiBilietą() { 
      if (bilietai > 0 ) {
        bilietai -= 1;
      }
    }
  }

   
  @context
  case class VažiuotiAutobusu(transportoPriemonė: TransportoPriemonė, vairuotojas: Vairuotojas,
    keleivis: Keleivis, atstumas: Double) {
  
    // Rolės    
    private val autobusoVairuotojas = vairuotojas
    private val autobusoKeleivis = keleivis  
    private val autobusas  = transportoPriemonė

    role autobusas {
      def važiuoti {
        self.kilometražas += atstumas
        vairuotojas.vairuoti
        autobusoKeleivis.važiuoti
      }
    }	
    role autobusoVairuotojas {
      def vairuoti {	
        self.stažas += atstumas
      }
    }    	
    role autobusoKeleivis {
      def važiuoti {
        if (self.bilietai > 0) {
          self.pasižymėtiBilietą()
        } else {			// sumokėti už bilietą vairuotojui
          self.pinigai -= autobusas.kelionėsKaina
            autobusoVairuotojas.pinigai += autobusas.kelionėsKaina
        }		
      }	  
    }

    autobusas.važiuoti
  }   
  
  @context
  case class VažiuotiDviračiu(transportoPriemonė: TransportoPriemonė, 
    keleivis: Keleivis, atstumas: Double) {    
    // Rolės        
    private val dviratininkas = keleivis  
    private val dviratis  = transportoPriemonė

    role dviratininkas{
      def pasiimtiDviratį{
        self.pinigai -= dviratis.kelionėsKaina
        dviratis.važiuoti
      }	        
    }	
    role dviratis {
      def važiuoti {        
        self.kilometražas += atstumas
      }
    }
    dviratininkas.pasiimtiDviratį	
  }
  
  // Test    
  val dviratis = TransportoPriemonė("dviratis", 1234, 2)  
  val keleivis = Keleivis("Petras", "Petraitis", 120, 0)
  
  VažiuotiDviračiu(dviratis, keleivis, 50) 
  keleivis.pinigai === 120 - 2
}