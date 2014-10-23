import org.scalatest.{Inside, Matchers, FlatSpec}
import sqlnorm.MysqlDialect
import sqlnorm.Ast._

class SqlParserSpec extends FlatSpec with Matchers with Inside {
  val q0 = "select person_id, birth_year from person where person.birth_year > 2000 limit 100"

  "MySQL parser" should "parse select statement" in {
    MysqlDialect.parser.parseAllWith(MysqlDialect.parser.stmt, q0) map {
      case Select(projection, tableRefs, where, None, None, Some(limit)) =>
        inside(projection) {
          case List(Named(col1, None, _), Named(col2, None, _)) =>
            col1 shouldBe "person_id"
            col2 shouldBe "birth_year"
          case _ => fail("Failed to match projection")
        }
        inside(tableRefs) {
          case List(ConcreteTable(Table(t1, None), List())) =>
            t1 shouldBe "person"
          case _ => fail("Failed to match table refs")
        }
        inside(where) {
          case Some(Where(Comparison2(Column(lhs, _), op, Constant(_, rhs)))) =>
            lhs shouldBe "birth_year"
            op shouldBe Gt
            rhs shouldBe 2000
          case _ => fail("Failed to match where clause")
        }
        inside(limit) {
          case Limit(Left(count), None) => count shouldBe 100
          case _ => fail("Failed to match limit")
        }
      case _ => fail("Failed to parse q0")
    }
  }

}
