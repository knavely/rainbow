name := "rainbow"

version := "1.0"

scalaVersion := "2.11.2"

resolvers += Resolver.bintrayRepo("rallyhealth", "ivy-scala-libs")

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.1"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.13.4",
  "org.scalanlp" %% "breeze" % "0.13.2",
  "com.rallyhealth" % "scalacheck-ops_1-14_2.11" % "2.1.0")
