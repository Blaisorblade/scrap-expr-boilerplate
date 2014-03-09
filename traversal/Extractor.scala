package traversal

abstract class Extractor[A, B] { def unapply(x: A): B }

object Extractor {
  def extractor[A, B](f: A => B) = new Extractor[A, B] { def unapply(x: A): B = f(x) }
  def textractor[T]: (Any => Option[T]) => Extractor[Any,Option[T]] = extractor[Any, Option[T]] _
}
