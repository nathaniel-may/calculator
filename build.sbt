scalaVersion := "2.12.8"
name := "calculator"
version := "1.0"

lazy val calculator = (project in file("."))
  .settings(
    scalacOptions ++= Seq(
      "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
      "-explaintypes",                     // Explain type errors in more detail.
      "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
      "-language:higherKinds",             // Allow higher-kinded types
      "-language:postfixOps",              // Allow postfix operators
      "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
      "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
      "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
      "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
      "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
      "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
      "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
      "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
      "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
      "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
      "-Xlint:option-implicit",            // Option.apply used implicit view.
      "-Xlint:package-object-classes",     // Class or object defined in package object.
      "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
      "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
      "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
      "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
      "-Xlint:unsound-match",              // Pattern match may not be typesafe.
      "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
      "-Ypartial-unification",             // Enable partial unification in type constructor inference
      "-Ywarn-dead-code",                  // Warn when dead code is identified.
      "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
      "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
      "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
      "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
      "-Ywarn-numeric-widen",              // Warn when numerics are widened.
      "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
      "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
      "-Ywarn-unused:locals",              // Warn if a local definition is unused.
      "-Ywarn-unused:params",              // Warn if a value parameter is unused.
      "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
      "-Ywarn-unused:privates",            // Warn if a private member is unused.
      "-Ywarn-value-discard" ),            // Warn when non-Unit expression results are unused.

    resolvers += "jitpack" at "https://jitpack.io",

    libraryDependencies += "org.typelevel"    %% "cats-core"   % "1.6.0",
    libraryDependencies += "org.typelevel"    %% "cats-effect" % "1.3.1",
    libraryDependencies += "com.github.scopt" %% "scopt"       % "3.7.1",

    libraryDependencies += "com.github.nathaniel-may" %  "functional-shuffle" % "1.0.0"  % "test",
    libraryDependencies += "org.scalatest"            %% "scalatest"          % "3.0.5"  % "test",
    libraryDependencies += "org.scalacheck"           %% "scalacheck"         % "1.14.0" % "test",

    testOptions in Test += Tests.Argument("-oD"),
    scalacOptions in (Compile, console) ~= (_ filterNot (_ == "-Xfatal-warnings") filterNot (_ == "-Ywarn-unused:imports")),
    scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,

    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", _ @ _*) => MergeStrategy.discard
      case _                            => MergeStrategy.first
    },

    assemblyJarName in assembly := "calc.jar"
  )