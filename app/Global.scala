import play.api._
import play.api.mvc._
import play.api.mvc.Results._

object Global extends GlobalSettings {

  override def onError(request: RequestHeader, ex: Throwable) = {
    InternalServerError(views.html.errors.errorInternal(ex))
  }
  
  // 404 - page not found error
  override def onHandlerNotFound(request: RequestHeader): Result = {
    NotFound(views.html.errors.errorOnHandlerNotFound(request))
  }
  
  
  override def onBadRequest(request: RequestHeader, error: String) = {
    BadRequest("Bad Request: " + error)
  } 
    
}