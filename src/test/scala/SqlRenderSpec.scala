import org.scalatest.{FlatSpec, Matchers}
import sqlnorm.SqlParser.MysqlParser
import sqlnorm._

class SqlServerRenderSpec extends SqlRenderSpec[SqlServerDialect] {

  "SQL Server renderer" should "translate equivalent select" in {
    Seq("=", "<>", "<", ">", "<=", ">=") foreach { operator =>
      val q = s"select person_id from person where birth_year $operator 2000"
      q shouldRender q
    }
  }

  it should "translate limit to top" in {
    val q = "select person_id, birth_year from person limit 100"
    val expected = "select top (100) person_id, birth_year from person"
    q shouldRender expected
  }

}

abstract class SqlRenderSpec[T <: Dialect] extends FlatSpec with Matchers {

  def parse(query: String) = MysqlParser.parseAllWith(MysqlParser.stmt, query)

  implicit class ShouldRender(q1: String) {
    def shouldRender(q2: String)(implicit renderer: SqlRenderer[T]) = parse(q1) map {statement =>
      renderer.render(statement) shouldBe q2
    }
  }

}