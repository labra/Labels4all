package controllers
import org.specs2._

import play.api.test._
import play.api.test.Helpers._
import models._
class ApplicationSpec extends Specification { def is = 
  
  "Specification of Labels4all Admin" 						^
  															p^
  "The application should" 									^
  	"authenticate"											!e1^
  															end

  def e1 = {
		val view = ViewTranslation('0', "http://xmlns.com/foaf/0.1/Person","es","Persona",1)
		view.label must beEqualTo("Persona")
		
		

  }
  
  
}