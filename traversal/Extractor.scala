package traversal

abstract class Extractor[A, B] { def unapply(x: A): B }

object Extractor {
  def extractor[A, B](f: A => B) = new Extractor[A, B] { def unapply(x: A): B = f(x) }
  def textractor[T]: (Any => Option[T]) => Extractor[Any,Option[T]] = extractor[Any, Option[T]] _
}

import Extractor._

object ExtractorExamples extends App {
  trait Exp
  case object Foo extends Exp
  val Exp = extractor[Any, Option[Exp]] { case e : Exp => Some(e); case _ => None }
  assert ((Foo match { case Exp(e) => e }) == Foo)
  val Exp2 = textractor[Exp] { case e : Exp => Some(e); case _ => None }
  assert ((Foo match { case Exp2(e) => e }) == Foo)
}