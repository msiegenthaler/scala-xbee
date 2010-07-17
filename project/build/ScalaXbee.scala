import sbt._

class ScalaXbeeProject(info: ProjectInfo) extends DefaultProject(info) with AutoCompilerPlugins {      
  val continuations = compilerPlugin("org.scala-lang.plugins" % "continuations" % "2.8.0")
  override def compileOptions = CompileOption("-P:continuations:enable") :: CompileOption("-unchecked") :: super.compileOptions.toList
  
  val logbackcore = "ch.qos.logback" % "logback-core" % "0.9.24"
  val logbackclassic = "ch.qos.logback" % "logback-classic" % "0.9.24"
  
  override def testClasspath = super.testClasspath +++ ("lib-test" / "scalabase_2.8.0-2.0.0-SNAPSHOT-test.jar") 
  val scalatest = "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.final-SNAPSHOT" % "test"
  val toolsSnapshot = ScalaToolsSnapshots
}
