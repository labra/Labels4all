package controllers.api.v1

import play.api._
import play.api.mvc._
import models._

import controllers.api.json.responses.JSend

import play.api.libs.json.{Json, JsNull}

object Jobs extends Controller {
    
    def redeemTicket(ticket: Long) = Action {
        Job.redeemTicket(ticket) match {
            case Some(job) => {
                job.result match {
                    case Some(res) => Ok(JSend.success(res))
                    case None => Accepted(JSend.success(Json.obj("job-status" -> "in progress")))
                }
            }
            case None => NotFound(JSend.fail(JsNull))
        }
    }
}