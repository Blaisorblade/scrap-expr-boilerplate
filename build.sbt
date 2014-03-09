scalaVersion := "2.10.3"

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test-src"

javaSource in Compile := baseDirectory.value / "src"

javaSource in Test := baseDirectory.value / "test-src"
