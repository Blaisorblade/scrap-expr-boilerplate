package traversal

//Using reflectiveCopy to implement bottom-up rewrites on typed trees.
//We assume that these rewrites are type-preserving.
trait TypedBaseLang {
  abstract class Exp[T] extends Product

  case class Const[T](t: T) extends Exp[T]
  case class Plus(a: Exp[Int], b: Exp[Int]) extends Exp[Int]
}

object TypedTraversalExample extends TypedTraversal with TypedBaseLang {
  val Exp = Extractor.textractor[Exp[_]] { case e: Exp[_] => Some(e); case _ => None }
}
