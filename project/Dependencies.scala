import sbt._

object Dependencies {
  val cats_version      = "2.2.0"
  val scalaBinVersion   = "${scala.version}"

  val munit             = "org.scalameta" %% "munit" % "0.7.12" % Test
  val scalatest         = "org.scalatest" % s"scalatest_$scalaBinVersion" % "3.0.3" % Test
  val scala_library     = "org.scala-lang" % "scala-library" % "${scala.version}"
  val junit             = "junit" % "junit" % "4.11" % "test"
  val cats_laws         = "org.typelevel" % s"cats-laws_$scalaBinVersion" % cats_version
  val cats_effect_laws  = "org.typelevel" % s"cats-effect-laws_$scalaBinVersion" % cats_version
  val cats_testkit      = "org.typelevel" % s"cats-testkit_$scalaBinVersion" % cats_version
  val cats_free         = "org.typelevel" % s"cats-free_$scalaBinVersion" % cats_version
  val cats_core         = "org.typelevel" % s"cats-core_$scalaBinVersion" % cats_version
  val cats_effect       = "org.typelevel" % s"cats-effect_$scalaBinVersion" % cats_version

}
