package models
import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._


class LanguageSpec extends Specification {

  "create a language" in {
    running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val langCode = "es"
      val langName = "Spanish"
      Language.create(langCode,langName)
      val idLang = Language.lookup(langCode)
	  idLang must beSome
	  
	  
	  
    }
   }


}