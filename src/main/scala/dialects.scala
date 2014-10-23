package sqlnorm

import Ast._
import java.sql.{Types => JdbcTypes}

trait Dialect {
  def parser: SqlParser
}

object Dialect {
  def choose(driver: String): Dialect = {
    if (driver.toLowerCase.contains("mysql")) MysqlDialect
    else if (driver.toLowerCase.contains("postgresql")) PostgresqlDialect
    else GenericDialect
  }
}

object MysqlDialect extends Dialect {
  val parser = MysqlParser

  object MysqlParser extends SqlParser {
    import scala.reflect.runtime.universe.typeOf

    override def insert = "insert".i <~ opt("ignore".i)
    override def update = "update".i <~ opt("ignore".i)

    override lazy val insertStmt = insertSyntax ~ opt(onDuplicateKey) ^^ {
      case t ~ cols ~ vals ~ None => Insert(t, cols, vals)
      case t ~ cols ~ vals ~ Some(as) =>
        Composed(Insert(t, cols, vals), Update(t :: Nil, as, None, None, None))
    }

    lazy val onDuplicateKey =
      "on".i ~> "duplicate".i ~> "key".i ~> "update".i ~> repsep(assignment, ",")

    override def quoteChar = ("\"" | "`")

    override def extraTerms = MysqlParser.interval

    override def dataTypes = List(
      precision1("binary")
      , precision1("char")
      , precision1("varchar")
      , precision0("tinytext")
      , precision0("text")
      , precision0("blob")
      , precision0("mediumtext")
      , precision0("mediumblob")
      , precision0("longtext")
      , precision0("longblob")
      , precision1("tinyint")
      , precision1("smallint")
      , precision1("mediumint")
      , precision1("int")
      , precision1("bigint")
      , precision0("float")
      , precision2("double")
      , precision2("decimal")
      , precision0("date")
      , precision0("datetime")
      , precision0("timestamp")
      , precision0("time")
      , precision0("signed")
      , precision0("unsigned")
    )

    def precision0(name: String) = name.i ^^ (n => DataType(n))
    def precision1(name: String) = (
      name.i ~ "(" ~ integer ~ ")" ^^ { case n ~ _ ~ l ~ _ => DataType(n, List(l)) }
        | precision0(name)
      )
    def precision2(name: String) = (
      name.i ~ "(" ~ integer ~ "," ~ integer ~ ")" ^^ { case n ~ _ ~ l1 ~ _ ~ l2 ~ _ => DataType(n, List(l1, l2)) }
        | precision1(name)
        | precision0(name)
      )

    lazy val intervalAmount = opt("'") ~> numericLit <~ opt("'")
    lazy val interval = "interval".i ~> intervalAmount ~ timeUnit ^^ { case x ~ _ => const((typeOf[java.util.Date], JdbcTypes.TIMESTAMP), x) }

    lazy val timeUnit = (
      "microsecond".i
        | "second".i
        | "minute".i
        | "hour".i
        | "day".i
        | "week".i
        | "month".i
        | "quarter".i
        | "year".i
      )
  }
}

object GenericDialect extends Dialect {
  val parser = new SqlParser {}
}

object PostgresqlDialect extends Dialect {
  val parser = new SqlParser {}
}

trait SqlServerDialect extends Dialect

object SqlServerDialect extends SqlServerDialect  {
  val parser = SqlServerParser

  // FIXME Implement SQL Server parser
  object SqlServerParser extends SqlParser
}
