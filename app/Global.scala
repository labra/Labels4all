import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import play.api.libs.json.Json

import scala.concurrent.Future

import controllers.api.json.responses.JSend


object Global extends GlobalSettings {
    override def onBadRequest(request: RequestHeader, error: String) = {
        if(request.path.startsWith("/api")) {
            Future.successful(BadRequest(JSend.error(error)))
        } else {
            Future.successful(BadRequest("Bad Request: " + error))
        }
    }
}