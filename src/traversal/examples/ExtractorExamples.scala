package traversal
package examples

import Extractor._

object ExtractorExamples extends App {
  trait Exp
  case object Foo extends Exp
  val Exp = extractor[Any, Option[Exp]] { case e : Exp => Some(e); case _ => None }
  assert ((Foo match { case Exp(e) => e }) == Foo)
  val Exp2 = textractor[Exp] { case e : Exp => Some(e); case _ => None }
  assert ((Foo match { case Exp2(e) => e }) == Foo)
}
