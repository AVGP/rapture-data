object project extends ProjectSettings {
  def scalaVersion = "2.11.2"
  def version = "1.0.2"
  def name = "data"
  def description = "Rapture Data provides a basis for working with structured data, such as JSON and XML"
  
  def dependencies = Seq(
    "core" -> "1.0.0"
  )
  
  def thirdPartyDependencies = Nil

  def imports = Seq(
    "rapture.core._",
    "rapture.data._"
  )
}
