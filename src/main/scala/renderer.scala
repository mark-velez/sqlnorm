package sqlnorm

import sqlnorm.Ast._

/**
 * SqlRenderer is a type class which allows us to derive a renderer from a dialect
 * @tparam U Concrete dialect
 */
trait SqlRenderer[U <: Dialect] {

  // FIXME do exhaustive matches on all components

  def render[T](stmt: Statement[T]): String = stmt match {
    case Select(projection, tableRefs, optWhere, optGroupBy, optOrderBy, optLimit) =>
      val whereClause = optWhere.map(render[T]).getOrElse("")
      val limit = optLimit.map(render[T]).getOrElse("")
      val tokens = Seq("select", render(projection), "from", render(tableRefs), whereClause, limit)
      tokens.reduceLeft[String]((agg, token) => if (token.isEmpty) agg else s"$agg $token")
  }

  protected def render[T](projection: List[Named[T]]): String = {
    val columns = projection map {
      case Named(name, Some(alias), _) => s"$name as $alias"
      case Named(name, None, _) => name
    }
    columns.mkString(", ")
  }

  protected def render[T](references: List[TableReference[T]])(implicit i1: DummyImplicit): String =
    references.map(_.name).mkString(", ")

  def render(operator: Operator2): String = operator match {
    case `Eq` => "="
    case `Neq` => "<>"
    case `Lt` => "<"
    case `Gt` => ">"
    case `Le` => "<="
    case `Ge` => ">="
    case `In` => "in"
    case `NotIn` => "not in"
    case `Like` => "like"
  }

  def render[T](term: Term[T]): String = term match {
    case Column(name, _) => name
    case Constant(_, value) => value.toString
  }

  def render[T](expr: Expr[T]): String = expr match {
    case Comparison2(term1, op, term2) => s"${render(term1)} ${render(op)} ${render(term2)}"
  }

  protected def render[T](where: Where[T]): String = where match {
    case Where(expr) => s"where ${render(expr)}"
  }

  protected def render[T](limit: Limit[T]): String = s"fetch first num_rows {limit.count.left.get.toString} rows only"

}

object SqlRenderer {

  implicit object SqlServerRenderer extends SqlRenderer[SqlServerDialect] {

    override def render[T](stmt: Statement[T]) = stmt match {
      case Select(projection, tableRefs, optWhere, optGroupBy, optOrderBy, optLimit) =>
        val whereClause = optWhere.map(render[T]).getOrElse("")
        val limit = optLimit.map(render[T]).getOrElse("")
        val tokens = Seq("select", limit, render(projection), "from", render(tableRefs), whereClause)
        tokens.reduceLeft[String]((agg, token) => if (token.isEmpty) agg else s"$agg $token")
    }

    override def render[T](limit: Limit[T]) = s"top (${limit.count.left.get.toString})"

  }

}