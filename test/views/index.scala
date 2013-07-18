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
	val html = views.html.about()
    contentType(html) must equalTo("text/html")
  }
  
  def e2 = {
	val html = views.html.about()
    contentType(html) must equalTo("text/html")
  }
  
  
}