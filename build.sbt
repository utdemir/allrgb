lazy val root = (project in file(".")).
  settings(
    name := "hello",
    version := "1.0",
    scalaVersion := "2.11.8"
  )

 libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-core" % "2.1.7"
 sourceDirectories in Compile += new File("src")
