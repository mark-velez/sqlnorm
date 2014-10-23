package sqlnorm

import sqlnorm.Ast._

trait SqlRenderer[U <: Dialect] {
  def render[T](stmt: Statement[T]): Option[String] = stmt match {
    case Select(projection, tableRefs, optWhere, groupBy, orderBy, optLimit) =>
      val whereClause = optWhere match {
        case Some(where) => s"where ${render(where)}"
        case _ => ""
      }
      val limitExpr = optLimit match {
        case Some(limit) => s"limit ${render(limit)}"
        case _ => ""
      }
      Some(s"select ${render(projection)} from ${render(tableRefs)} $whereClause $limitExpr")
    case _ => None
  }

  protected def render[T](projection: List[Named[T]]) = {
    val terms = projection map {
      case Named(name, Some(alias), _) => s"$name as $alias"
      case Named(name, None, _)        => name
      case _ => ""
    }
    terms.mkString(", ")
  }

  protected def render[T](references: List[TableReference[T]])(implicit i1:DummyImplicit) =
    if (references.size > 0) references.map(_.name).mkString(", ") else ""

  protected def render[T](where: Where[T]) = ???

  protected def render[T](limit: Limit[T]) = ???

}

object SqlRenderer {
  implicit object SqlServerRenderer extends SqlRenderer[SqlServerDialect] {
    override def render[T](stmt: Statement[T]): Option[String] = stmt match {
      case Select(projection, tableRefs, where, groupBy, orderBy, Some(limit)) =>
        Some(s"select top (${limit.count.left.get}) ${render(projection)} from ${render(tableRefs)}")
      case _ => None
    }
  }
}