package controllers.api.v1

import play.api._
import play.api.mvc._
import models._

import models.dto.IRIWithLabels
import controllers.api.json.responses.JSend

import play.api.libs.json.{Json, JsNull, JsObject, JsValue}

object Vocabularies extends Controller {
    def insert() = Action(parse.json) { request =>
        checkFormat(processBody(request.body)) match {
            case Left(res) => res
            case Right(p) => {
                IRIWithLabels.fromXML(p.get("file").get, p.get("defaultLang")) match {
                    case None => BadRequest(JSend.error("Unable to parse XML data"))
                    case Some(iris) =>  {
                        val ticket = IRI.createManyAndAddLabels(iris)
                        ticket match {
                            case Some(t) => Created(
                                                JSend.success(Json.obj("ticket" -> t)))
                                                     .withHeaders(LOCATION -> "/api/v1/tickets/".concat(t.toString))
                            case None => InternalServerError(JSend.error("Could not create ticket"))
                        }
                    }
                }
            }
        }
    }
    
    def export() = Action {
        NotImplemented
    }
    
    def stats() = Action {
        NotImplemented
    }
    
    private[this] def processBody(body: JsValue): Option[Map[String, Option[String]]] = {
        body match {
            case JsObject(fields) => {
                    Some(fields.map(e => (e._1, e._2.asOpt[String]))
                               .toMap)
            }
            case _ => None
        }
    }
    
    private[this] def checkFormat(body: Option[Map[String, Option[String]]]): Either[Result, Map[String, String]] = {
        body match {
            case None => Left(BadRequest(JSend.error("JSON object expected")))
            case Some(m) => {
                val p = m.collect { case (k, Some(v)) => k -> v}
                
                if(p.contains("format") && p("format").equalsIgnoreCase("rdf/xml") && p.contains("file"))
                    Right(p)
                else
                    Left(BadRequest(JSend.error("Missing required fields (either 'format' or 'file') or invalid format")))
            }
        }
    }
}