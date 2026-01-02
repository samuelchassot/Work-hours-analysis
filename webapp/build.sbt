name := "work-hours-app"
version := "0.1.0-SNAPSHOT"
scalaVersion := "3.7.4" 

enablePlugins(org.scalajs.sbtplugin.ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true

Compile / mainClass := Some("workhours.App")


// Put JS output in ./public/js so a dumb static server works
Compile / fastLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "public" / "js"
Compile / fullLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "public" / "js"

// ES modules so we can use <script type="module">
Compile / fastLinkJS / scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule))
Compile / fullLinkJS / scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule))

libraryDependencies ++= Seq(
    "com.raquo" %%% "laminar" % "17.2.1",        // :contentReference[oaicite:9]{index=9}
    "org.scala-js" %%% "scalajs-dom" % "2.8.1"   // :contentReference[oaicite:10]{index=10}
)