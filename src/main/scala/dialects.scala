package sqlnorm

sealed trait Dialect

object Dialect {
  def choose(driver: String): Dialect = {
    if (driver.toLowerCase.contains("mysql")) MysqlDialect
    else if (driver.toLowerCase.contains("postgresql")) PostgresqlDialect
    else GenericDialect
  }
}

trait GenericDialect extends Dialect

case object GenericDialect extends Dialect

trait MysqlDialect extends Dialect

case object MysqlDialect extends Dialect

trait PostgresqlDialect extends Dialect

case object PostgresqlDialect extends Dialect

trait SqlServerDialect extends Dialect

case object SqlServerDialect extends Dialect
