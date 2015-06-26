package ctx

import play.api.Play.current
import play.api.libs.concurrent.Akka

import scala.concurrent.ExecutionContext

object Contexts {
  implicit val jobsDisp: ExecutionContext = Akka.system.dispatchers.lookup("jobs-dispatcher")
}