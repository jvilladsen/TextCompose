// Assuming use of sbt version 0.13.1 (or later?)

name := "Writesetter"

version := "0.7.1"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
    "com.itextpdf" % "itextpdf" % "5.4.1",
    "org.scala-lang" % "scala-swing" % "2.10.2",
    "org.bouncycastle" % "bcprov-jdk15on" % "1.49",
    "org.bouncycastle" % "bcpkix-jdk15on" % "1.49",
    "net.sf.jazzy" % "jazzy-core" % "0.5.2")

mainClass := Some("writesetter.startup.Launch")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

fork := true

// appbundle
    
seq(appbundle.settings: _*)

appbundle.name := name.value

appbundle.version := name.value + " " + version.value + " (c) 2013 J S Villadsen"

appbundle.javaOptions += "-Xmx1024m"

appbundle.icon := Some(file("src/main/resources/Writesetter.icns"))

appbundle.mainClass := Some("writesetter.startup.Launch")

appbundle.documents := Seq(appbundle.Document(
    "Writesetter source",
    role = appbundle.Document.Editor,
    mimeTypes = Seq("text/plain"),
    extensions = Seq("wr"),
    icon = Some(file("src/main/resources/Writesetter_doc.icns"))))

// one-jar

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

mainClass in oneJar := Some("writesetter.startup.Launch")
