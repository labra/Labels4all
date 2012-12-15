package controllers
import org.specs2._

import play.api.test._
import play.api.test.Helpers._
import models._


class AdminSpec extends Specification { def is = 
  
  "Specification of Labels4all Admin" 						^
  															p^
  "The application should" 									^
  	"authenticate"											!e1^
  															end

  def e1 = {
   true must be_===(true)
  }
  
  
}