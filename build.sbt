scalaVersion := "2.12.8"
name := "calculator"
version := "1.0"

lazy val calculator = (project in file("."))
  .settings(
    scalacOptions += "-Ypartial-unification",
    scalacOptions += "-language:postfixOps",

    resolvers += "jitpack" at "https://jitpack.io",

    libraryDependencies += "org.typelevel"    %% "cats-core"   % "1.6.0",
    libraryDependencies += "org.typelevel"    %% "cats-effect" % "1.3.1",

    libraryDependencies += "com.github.nathaniel-may" %  "functional-shuffle" % "1.0.0"  % "test",
    libraryDependencies += "org.scalatest"            %% "scalatest"          % "3.0.5"  % "test",
    libraryDependencies += "org.scalacheck"           %% "scalacheck"         % "1.14.0" % "test",

    testOptions in Test += Tests.Argument("-oD"),
  )