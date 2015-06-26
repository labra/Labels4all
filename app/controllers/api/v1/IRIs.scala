package controllers.api.v1

import play.api._
import play.api.mvc._
import models._
import models.json.Writers._

import models.dto.IRIWithLabels
import controllers.api.json.responses.JSend

import play.api.libs.json.{Json, JsNull, JsObject}

object IRIs extends Controller {
        
        def details(iriName: String, lang: List[String]) = Action {
            val r = 
                if(lang.isEmpty) 
                    IRI.mostVotedLabelEveryLanguage(iriName)
                else 
                    IRI.mostVotedLabelSelectedLanguages(iriName, lang)
            
            r match {
                case None => NotFound(JSend.fail(JsNull))
                case Some(iri) => {
                    Ok(JSend.success(Json.toJson(iri)))
                }
            }
        }
        
        def detailsWithLang(iriName: String, langCode: String) = Action {
            IRI.allLabelsSelectedLanguage(iriName, langCode) match {
                case None => NotFound(JSend.fail(JsNull))
                case Some(iri) => {
                    Ok(JSend.success(Json.toJson(iri)))
                }
            }
        }
        
        def create(iriName: String) = Action(parse.json) { request =>
            request.body match {
                case JsObject(fields) => {                    
                    val m = fields.map(e => (e._1, e._2.asOpt[Seq[String]]))
                                  .collect { case (lang, Some(labels)) => lang -> labels }
                                  .toMap

                    if(!m.isEmpty) {
                        val inserted = IRI.createAndAddLabels(IRIWithLabels(iriName, m))
                        Ok(JSend.success(Json.obj("inserted" -> inserted.size)))
                    } else {
                        BadRequest(JSend.error("Empty labels map"))
                    }
                }
                case _ => BadRequest(JSend.error("JSON object expected"))
            }
        }
}