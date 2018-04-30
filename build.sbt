name := "sphynx"

version := "0.1"

scalaVersion := "2.12.4"

scalacOptions ++= Seq(
  "-feature",
  "-language:higherKinds",
  "-language:existentials",
  "-language:implicitConversions",
  "-language:postfixOps"
)

val CompileTime = config("compile-time").hide

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.1.0",
  "com.github.mpilquist" %% "simulacrum" % "0.11.0"  % CompileTime,
  "org.typelevel" %% "cats-effect" % "1.0.0-RC" % Test,
  "io.monix" %% "minitest" % "2.0.0" % Test
)


testFrameworks += new TestFramework("minitest.runner.Framework")

ivyConfigurations += CompileTime

unmanagedClasspath in Compile ++=
  update.value.select(configurationFilter(CompileTime.name))

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.5")
