val scala3Version = "3.4.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "hanoi",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
        libraryDependencies += "org.scalafx" %% "scalafx" % "22.0.0-R33"
      

    scalacOptions ++= Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-feature")
    
  
  

    fork := true
