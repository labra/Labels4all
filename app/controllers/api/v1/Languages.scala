package controllers.api.v1

import play.api._
import play.api.mvc._
import models._
import models.json.Writers._

import controllers.api.json.responses.JSend

import play.api.libs.json.{Json, JsNull}

object Languages extends Controller {
    
    def list(page: Int) = Action {
        Ok(JSend.success(Json.toJson(Language.all(page))))
    }
    
    def details(langCode: String) = Action {
        val res = Language.numberOfLabels(langCode)
        res._1 match {
            case Some(language) => Ok(JSend.success(Json.toJson(language, res._2)))
            case None => NotFound(JSend.fail(JsNull))
        } 
    }
}