package sqlnorm

case class DbConfig(url: String, driver: String, username: String, password: String, schema: Option[String]) {
  def getConnection = java.sql.DriverManager.getConnection(url, username, password)
}
