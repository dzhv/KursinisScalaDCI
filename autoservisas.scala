package scaladci
package examples
import org.specs2.mutable._

/*
  Autoserviso pavyzdys kursiniam darbui
  
  Džiugas Vyšniauskas
*/

class Autoservisas extends Specification {
  
  case class Automobilis(numeris: String, markė: String, pagaminimoData: DateTime, statusas: String, paskutinėsPeržiūrosData: DateTime) {  }
  case class Mechanikas(vardas: String, pavardė: String, telNr: String, darbai: ListBuffer[Automobilis]) {}

  "With various role syntaxes" >> {
   
    @context
    case class AtliktiDarbą(mechanikas: Mechanikas) {

      // Rolės
      private val meistras  = mechanikas
	  private val automobilis = mechanikas.darbai.take(1)       

      role meistras {
        def taisytiAutomobilį {
          automobilis.atsinaujinti
		  self.darbai = self.darbai.drop(1)
        }
      }

      role automobilis {
        def atsinaujinti {
          automobilis.paskutinėsPeržiūrosData = DateTime.Now
		  automobilis.statusas = "Tvarkingas"
        }
      }
    }


    // Test
    val automibilis = Automobilis("III222", "Mazda 5", DateTime.parse("2013-05-05"), "Sugadintas", DateTime.parse("2013-05-05"))
    val mechanikas = Mechanikas("Jonas", "Jonaitis", "8699999999", new ListBuffer(automobilis))

	AtliktiDarbą(mechanikas)
	automobilis.statusas === "Tvarkingas"
	mechanikas.darbai.length === 0
		
  }
}