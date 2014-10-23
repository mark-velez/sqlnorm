package sqlnorm

import java.sql._
import scala.reflect.runtime.universe.{Type, typeOf}

private[sqlnorm] object Jdbc {
   def unknownTerm = Ast.Column("unknown", Ast.Table("unknown", None))

  def withConnection[A](conn: Connection)(a: Connection => A): ?[A] = try { 
    a(conn).ok
  } catch {
    case e: SQLException => fail(e.getMessage)
  } finally { 
    conn.close 
  }

  // FIXME move to TypeMappings
  def mkType(className: String): Type = className match {
    case "java.lang.String" => typeOf[String]
    case "java.lang.Short" => typeOf[Short]
    case "java.lang.Integer" => typeOf[Int]
    case "java.lang.Long" => typeOf[Long]
    case "java.lang.Float" => typeOf[Float]
    case "java.lang.Double" => typeOf[Double]
    case "java.lang.Boolean" => typeOf[Boolean]
    case "java.lang.Byte" => typeOf[Byte]
    case "java.sql.Timestamp" => typeOf[java.sql.Timestamp]
    case "java.sql.Date" => typeOf[java.sql.Date]
    case "java.sql.Time" => typeOf[java.sql.Time]
    case "byte[]" => typeOf[java.sql.Blob]
    case "[B" => typeOf[java.sql.Blob]
    case "byte" => typeOf[Byte]
    case "java.math.BigDecimal" => typeOf[scala.math.BigDecimal]
    case x => sys.error("Unknown type " + x)
  }
}

private [sqlnorm] object TypeMappings {
  import java.sql.Types._

  /* a mapping from java.sql.Types.* values to their getFoo/setFoo names */
  final val setterGetterNames = Map(
      ARRAY         -> "Array"  
    , BIGINT	    -> "Long"   
    , BINARY	    -> "Bytes"  
    , BIT	    -> "Boolean"
    , BLOB	    -> "Blob"   
    , BOOLEAN	    -> "Boolean"
    , CHAR	    -> "String"
    , CLOB	    -> "Clob"
    , DATALINK	    -> "URL"
    , DATE	    -> "Date"
    , DECIMAL	    -> "BigDecimal"
    , DOUBLE	    -> "Double"
    , FLOAT	    -> "Float"
    , INTEGER	    -> "Int"
    , JAVA_OBJECT   -> "Object"
    , LONGNVARCHAR  -> "String"
    , LONGVARBINARY -> "Blob"     // FIXME should be Bytes?
    , LONGVARCHAR   -> "String"
    , NCHAR	    -> "String"
    , NCLOB	    -> "NClob"
    , NUMERIC	    -> "BigDecimal"
    , NVARCHAR	    -> "String"
    , REAL	    -> "Float"
    , REF	    -> "Ref"
    , ROWID	    -> "RowId"
    , SMALLINT	    -> "Short"
    , SQLXML        -> "SQLXML"
    , TIME          -> "Time"
    , TIMESTAMP	    -> "Timestamp"
    , TINYINT	    -> "Byte"
    , VARBINARY	    -> "Bytes"
    , VARCHAR	    -> "String"
  )

  // FIXME this is dialect specific, move there. Or perhaps schemacrawler provides these?
  def arrayTypeName(tpe: Type) = 
    if (tpe =:= typeOf[String]) "varchar"
    else if (tpe =:= typeOf[Int]) "integer"
    else if (tpe =:= typeOf[Long]) "bigint"
    else sys.error("Unsupported array type " + tpe)
}
