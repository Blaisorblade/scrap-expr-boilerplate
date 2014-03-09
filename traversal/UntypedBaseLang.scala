package traversal

//Using reflectiveCopy to implement bottom-up rewrites on untyped trees.

trait UntypedBaseLang {
  trait Term extends Product
  case class Lit(i: Int) extends Term
  case class Plus(a: Term, b: Term) extends Term
}

object UntypedTraversalExample extends UntypedTraversal with UntypedBaseLang with App {
  val Term = Extractor.textractor[Term] { case e: Term => Some(e); case _ => None }

  //A very basic form of constant folding:
  val constantFoldingCore: Term => Term = {
    case Plus(Lit(a), Lit(b)) => Lit(a + b)
    case t => t
  }

  //Since traverse applies its tree bottom-up, this is able to do complete
  //constant-folding on a tree
  val constantFolding = traverse(constantFoldingCore)

  val tree = Plus(Plus(Lit(1), Lit(2)), Lit(3))
  println(s"Before constant folding: ${tree}")
  println(s"After constant folding: ${constantFolding(tree)}")
}
