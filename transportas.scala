package scaladci
package examples

/*
  DCI pavyzdys kursiniam darbui
  Džiugas Vyšniauskas
*/

class Transportas extends Specification {
  
  case class TransportoPriemonė(tipas: String, kilometražas: Double, kelionėsKaina: Double) {  }
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
	
	role autobuseKeleivis {
	  def važiuoti {
	    if (self.bilietai > 0) {
	      self.pasižymėtiBilietą()
		} else {			// sumokėti už bilietą vairuotojui
		  self.pinigai -= autobusas.kelionėsKaina
		  autobusoVairuotojas.pinigai += autobusas.kelionėsKaina
		}		
      }	  
	}

  }
  
  @context
  case class VažiuotiTaxi(transportoPriemonė: TransportoPriemonė, vairuotojas: Vairuotojas,
    keleivis: Keleivis, atstumas: Double) {
  
    // Rolės    
    private val taxiVairuotojas = vairuotojas
    private val taxiKeleivis = keleivis  
	private val taxi  = transportoPriemonė
		
	role taxi {
      def važiuoti {
        self.kilometražas += atstumas
        vairuotojas.vairuoti
		autobusoKeleivis.važiuoti
      }
    }
	
    role taxiVairuotojas{
      def vairuoti {	
	self.stažas += atstumas	
      }
    }    
	
	role taxiKeleivis {
	  def važiuoti {
	    self.pinigai -= taxi.kelionėsKaina * atstumas
		taxiVairuotojas.pinigai += taxi.kelionėsKaina * atstumas
      }	  
	}
	
  }

  // Test    
  val taxi = TransportoPriemonė("taxi", 1234, 2)
  val vairuotojas = Vairuotojas("Jonas", "Jonaitis", 100, 5000)
  val keleivis = Keleivis("Petras", "Petraitis", 120, 0)
  
  VažiuotiTaxi(taxi, vairuotojas, keleivis, 50)
  vairuotojas.stažas === 150
  keleivis.pinigai === 120 - 2 * 50
}