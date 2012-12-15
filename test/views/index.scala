package controllers
import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._

class IndexSpec extends Specification {

  "Specification of Views" 							^
  															p^
  "The application should" 									^
  	"render flash"											!e1^
  	"search person"											!e2^
  															end

  def e1 = {
    implicit val flash = new play.api.mvc.Flash(Map(("message","My flash"))) 
	val html = views.html.index(Seq(), Application.searchForm)
    contentType(html) must equalTo("text/html")
    contentAsString(html) must contain("My flash")
  }
  
  def e2 = {
    implicit val flash = new play.api.mvc.Flash(Map(("message","My flash"))) 
	val html = views.html.index(Seq(), Application.searchForm)
    contentType(html) must equalTo("text/html")
    contentAsString(html) must contain("My flash")
  }
  
  
}