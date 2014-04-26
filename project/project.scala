object project extends ProjectSettings {
  def scalaVersion = "2.10.4"
  def version = "0.10.0"
  def name = "data"
  def description = "Rapture Data provides a basis for working with structured data, such as JSON and XML"
  
  def dependencies = Seq(
    "core" -> "0.10.0"
  )
  
  def thirdPartyDependencies = Nil

  def imports = Seq(
    "rapture.core._",
    "rapture.data._",
    "strategy.throwExceptions"
  )
}
