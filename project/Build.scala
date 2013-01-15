import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "topichub"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      // Add your project dependencies here,
      "org.scalesxml" %% "scales-jaxen" % "0.4.3",
      "jaxen" % "jaxen" % "1.1.4",
      "postgresql" % "postgresql" % "9.1-901-1.jdbc4",
      "org.apache.httpcomponents" % "httpmime" % "4.1.2",
      "com.amazonaws" % "aws-java-sdk" % "1.3.27"
      //"be.objectify" %% "deadbolt-scala" % "2.0-SNAPSHOT"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here      
    )

}
