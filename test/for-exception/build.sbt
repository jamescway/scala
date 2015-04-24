val finagleVersion = "6.22.0"
val utilVersion = "6.22.1"

val derby = "org.apache.derby" % "derby" % "10.4.1.3"
def util(name: String) = "com.twitter" %% ("util-" + name) % utilVersion
def finagle(name: String) = "com.twitter" %% ("finagle-" + name) % finagleVersion

lazy val commonSettings = Seq(
  organization := "com.example",
  version := "0.1.0",
  scalaVersion := "2.10.4"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "for-exception",
    libraryDependencies ++= Seq(
      util("app"),
      util("core"))
  )