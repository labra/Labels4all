package models
import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import models._
import anorm._

class IRISpec extends Specification {

  "create IRI" in {
    running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val foafPerson = "http://xmlns.com/foaf/0.1/Person"
      IRI.create(foafPerson)
      val id = IRI.lookup(foafPerson)
	  id must beSome  
    }
   }

   "create the same IRI several times" in {
    running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val foafPerson = "http://xmlns.com/foaf/0.1/Person"
      IRI.create(foafPerson)
      IRI.create(foafPerson)
      IRI.create(foafPerson)
      val id = IRI.lookup(foafPerson)
      id must beSome  
      IRI.all().size must be_==(1)
    }
   }

  "create other IRI" in {
    running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val foafPerson = "http://xmlns.com/foaf/0.1/Person"
      val foafProject = "http://xmlns.com/foaf/0.1/Project"
      IRI.create(foafPerson)
      IRI.create(foafProject)
      val id1 = IRI.lookup(foafPerson)
      val id2 = IRI.lookup(foafProject)
	  id1 must beSome  
      id2 must beSome
      id1 must_!= id2
      IRI.all().size must be_==(2)
    }
   }

  "delete IRI" in {
    running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val foafPerson = "http://xmlns.com/foaf/0.1/Person"
      val foafProject = "http://xmlns.com/foaf/0.1/Project"
      IRI.create(foafPerson)
      IRI.create(foafProject)

      val id = IRI.lookup(foafPerson)
      id must beSome  
      IRI.delete(Id(id.get))
      val id2 = IRI.lookup(foafPerson)
      id2 must beNone
    }
   }
  
  "delete IRI with related translation" in {
    running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val foafPerson = "http://xmlns.com/foaf/0.1/Person"
      val foafProject = "http://xmlns.com/foaf/0.1/Project"
      IRI.create(foafPerson)
      IRI.create(foafProject)
    
      Language.create("es","castellano")
      val idLang = Language.lookup("es")
      val idIRI = IRI.lookup(foafPerson)
      
      Translation.create(idIRI.get,idLang.get,"Persona",1)
      IRI.all().size must be_==(2)
      //IRI.delete(Id(idIRI.get))
      IRI.all().size must be_==(2)
    }
   }

}