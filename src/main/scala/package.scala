package object sqlnorm
{
  import scala.language.experimental.macros
  import scala.language.implicitConversions

  // Internally ? is used to denote computations that may fail.
  private[sqlnorm] def fail[A](s: String, column: Int = 0, line: Int = 0): ?[A] =
  sqlnorm.Failure(s, column, line)
  private[sqlnorm] def ok[A](a: A): ?[A] = sqlnorm.Ok(a)
  private[sqlnorm] implicit class ResultOps[A](a: A) {
    def ok = sqlnorm.ok(a)
  }
  private[sqlnorm] implicit class ResultOptionOps[A](a: Option[A]) {
    def orFail(s: => String) = a map sqlnorm.ok getOrElse fail(s)
  }
  private[sqlnorm] def sequence[A](rs: List[?[A]]): ?[List[A]] =
  rs.foldRight(List[A]().ok) { (ra, ras) => for { as <- ras; a <- ra } yield a :: as }
  private[sqlnorm] def sequenceO[A](rs: Option[?[A]]): ?[Option[A]] =
  rs.foldRight(None.ok: ?[Option[A]]) { (ra, _) => for { a <- ra } yield Some(a) }
}

package sqlnorm {
  private[sqlnorm] abstract sealed class ?[+A] { self =>
    def map[B](f: A => B): ?[B]
    def flatMap[B](f: A => ?[B]): ?[B]
    def foreach[U](f: A => U): Unit
    def fold[B](ifFail: Failure[A] => B, f: A => B): B
    def getOrElse[B >: A](default: => B): B
    def filter(p: A => Boolean): ?[A]
    def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)
    class WithFilter(p: A => Boolean) {
      def map[B](f: A => B): ?[B] = self filter p map f
      def flatMap[B](f: A => ?[B]): ?[B] = self filter p flatMap f
      def foreach[U](f: A => U): Unit = self filter p foreach f
      def withFilter(q: A => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
    }
  }
  private[sqlnorm] final case class Ok[+A](a: A) extends ?[A] {
    def map[B](f: A => B) = Ok(f(a))
    def flatMap[B](f: A => ?[B]) = f(a)
    def foreach[U](f: A => U) = { f(a); () }
    def fold[B](ifFail: Failure[A] => B, f: A => B) = f(a)
    def getOrElse[B >: A](default: => B) = a
    def filter(p: A => Boolean) = if (p(a)) this else fail("filter on ?[_] failed")
  }
  private[sqlnorm] final case class Failure[+A](message: String, column: Int, line: Int) extends ?[A] {
    def map[B](f: A => B) = Failure(message, column, line)
    def flatMap[B](f: A => ?[B]) = Failure(message, column, line)
    def foreach[U](f: A => U) = ()
    def fold[B](ifFail: Failure[A] => B, f: A => B) = ifFail(this)
    def getOrElse[B >: A](default: => B) = default
    def filter(p: A => Boolean) = this
  }
}
