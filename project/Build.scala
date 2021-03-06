import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

    val appName         = "Labels4all"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      // Add your project dependencies here,
      "postgresql" % "postgresql" % "8.4-702.jdbc4" ,
      // Add your project dependencies here,
      "mysql" % "mysql-connector-java" % "5.1.18",
      jdbc,
      anorm
    )

    val main = play.Project(appName, appVersion, appDependencies).settings()
}
