import org.scalatest.{Matchers, FlatSpec}
import sqlnorm._

class SqlServerRenderSpec extends SqlRenderSpec[SqlServerDialect] {
  val q0 = "select person_id, birth_year from person limit 100"

  "SQL Server renderer" should "translate limit to top" in {
    val expected = "select top (100) person_id, birth_year from person"
    q0 shouldRender expected
  }
}

abstract class SqlRenderSpec[T <: Dialect] extends FlatSpec with Matchers {
  def parse(query: String) = GenericDialect.parser.parseAllWith(GenericDialect.parser.stmt, query)
  implicit class ShouldRender(q1: String) {
    def shouldRender(q2: String)(implicit renderer: SqlRenderer[T]) = parse(q1) map {statement =>
      val Some(result) = renderer.render(statement)
      result shouldBe q2
    }
  }
}