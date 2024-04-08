import Wart._

enablePlugins(ScalaJSPlugin)

val scala3Version = "3.3.3"
val directoryWatcherVersion = "0.18.0"

ThisBuild / scalaVersion     := "2.13.13"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "hkust-taco.github.io"
ThisBuild / organizationName := "HKUST-TACO"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:higherKinds",
  if (insideCI.value) "-Wconf:any:error"
  else                "-Wconf:any:warning",
)

lazy val root = project.in(file("."))
  .aggregate(hkmc2JS, hkmc2JVM, coreJS, coreJVM)
  .settings(
    publish := {},
    publishLocal := {},
  )

lazy val hkmc2 = crossProject(JSPlatform, JVMPlatform).in(file("hkmc2"))
  .settings(
    scalaVersion := scala3Version,
    sourceDirectory := baseDirectory.value.getParentFile()/"shared",
    watchSources += WatchSource(
      baseDirectory.value.getParentFile()/"shared"/"test"/"diff", "*.mls", NothingFilter),
    
    // scalacOptions ++= Seq("-indent", "-rewrite"),
    scalacOptions ++= Seq("-new-syntax", "-rewrite"),
    
    libraryDependencies += "io.methvin" % "directory-watcher" % directoryWatcherVersion,
    libraryDependencies += "io.methvin" %% "directory-watcher-better-files" % directoryWatcherVersion,
    libraryDependencies += "com.lihaoyi" %%% "fansi" % "0.4.0",
    // libraryDependencies += ("com.lihaoyi" %% "ammonite-ops" % "3.0.0-M1"),
    // libraryDependencies += "com.lihaoyi" %% "ammonite-ops" % "2.4.0",
    //.cross(CrossVersion.for3Use2_13),
    // libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.8.0",
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.3",
    
    libraryDependencies += "org.scalactic" %%% "scalactic" % "3.2.18",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.18" % "test",
    
    watchSources += WatchSource(
      // sourceDirectory.value.getParentFile().getParentFile()/"shared/src/test/mlscript", "*.mls", NothingFilter),
      baseDirectory.value.getParentFile()/"shared"/"test"/"mlscript", "*.mls", NothingFilter),
    watchSources += WatchSource(
      baseDirectory.value.getParentFile()/"shared"/"test"/"mlscript", "*.cmd", NothingFilter),
    
    Test/run/fork := true, // so that CTRL+C actually terminates the watcher
  )
  .jvmSettings(
    // libraryDependencies += "com.lihaoyi" %% "ammonite-ops" % "2.4.0",
  )
  .dependsOn(core)

lazy val hkmc2JVM = hkmc2.jvm
lazy val hkmc2JS = hkmc2.js

lazy val core = crossProject(JSPlatform, JVMPlatform).in(file("core"))
  .settings(
    sourceDirectory := baseDirectory.value.getParentFile()/"shared",
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
