package traversal

import scala.language.{ higherKinds, implicitConversions, postfixOps }

object Util {
  def count(amount: Int, noun: String): String = {
    (noun, amount) match {
      case (_, 1) => s"${amount} ${noun}"
      case (_, _) => s"${amount} ${noun}s"
    }
  }
}

import Util._

trait Reflection {
  /**
   * Allow functional update on arbitrary case classes. Call it with an
   * instance of a case class and its updated children.
   * Only works on case classes, or classes having an appropriate copy method.
   * The case class
   * @param t an instance of a case class
   */
  def reflectiveCopy[T <: Product](t: T, args: Any*): T = {
    val clazz = t.getClass
    if (args.length == t.productArity) {
      if (t.productArity == 0) {
        return t
      } else {
        val copyMethodOpt = clazz.getMethods filter (_.getName == "copy") headOption

        (copyMethodOpt getOrElse (
          throw new RuntimeException(s"No 'copy' method found in reflectiveCopy for ${t} of type ${clazz}")) invoke (t,
          args.toArray.asInstanceOf[Array[_ <: AnyRef]]: _*)).asInstanceOf[T]
      }
    } else {
      arityError(t.productArity, args.length)
    }
  }

  def arityError(expected: Int, given: Int) =
    throw new IllegalArgumentException(s"${count(expected, "argument")} expected but ${given} given")
}

//Using reflectiveCopy to implement bottom-up rewrites on untyped trees.

trait UntypedLang {
  trait Term extends Product
  case class Lit(i: Int) extends Term
  case class Plus(a: Term, b: Term) extends Term
}

trait UntypedTraversal extends UntypedLang with Reflection {
  /**
   * Take a transformer and a term, and apply transformer to each subterm of term. 
   * @param transformer
   */
  def mapSubtrees(transformer: Term => Term): Term => Term =
    term => {
      val subterms = term.productIterator.toList map {
        //The pattern matching cannot distinguish this.Term from (something else).Term.
        //Won't be a problem as long as you don't mix different Terms in the same tree.
        case subTerm: Term @unchecked => transformer(subTerm)
        case notTerm => notTerm
      }
      reflectiveCopy(term, subterms: _*)
    }

  /**
   * Apply transformer to a Term bottom-up: transformer is applied to each leave,
   * then the parent node is rebuilt with the transformed leaves, then the
   * transformer is applied to the newly constructed nodes, and so forth.
   * The traversal algorithm is the same as a fold.
   * 
   * If you want to implement a rewrite system, this might not be enough — you
   * might need to implement fix-point iteration, if a single rule needs to be
   * applied more than once in the same position. Since in my experience most
   * rules must be applied at most once, this is left to the rules themselves.
   * 
   * Beta-reduction is a typical example of a rule needing fixpoint iteration.  
   */ 
  def traverse(transformer: Term => Term): Term => Term =
    term =>
      transformer(mapSubtrees(traverse(transformer))(term))
  /*
   * Cai: you might want to compare the above to this equation from "Bananas in space ...", page 325.
   * cata phi (In x) = phi (map (cata phi) x)
   * The traversal algorithm is the same, but here we're restricted to
   * rebuilding the same kind of result, and the code is not actually polymorphic.
   * In exchange, we don't need to encode recursive datatypes specially.
   */
}

object UntypedTraversalExample extends UntypedTraversal with App {
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

//Using reflectiveCopy to implement bottom-up rewrites on typed trees.
//We assume that these rewrites are type-preserving.
trait TypedBaseLang {
  abstract class Exp[T] extends Product

  case class Const[T](t: T) extends Exp[T]
  case class Plus(a: Exp[Int], b: Exp[Int]) extends Exp[Int]
}


/**
 * Represent in Scala a particular kind of polymorphic functions called natural
 * transformations.
 * A natural transformation from A to B has type
 * forall T. A[T] => B[T].
 * This definition is also present in Scalaz/shapeless.
 * 
 * A type-preserving rewrite rule can be given type Exp ~> Exp.
 */ 
trait ~>[-A[_], +B[_]] {
  def apply[T](a: A[T]): B[T]
}

//A version of UntypedTraversal working on typed trees. 
trait TypedTraversal extends TypedBaseLang with Reflection {
  def mapSubtrees(transformer: Exp ~> Exp) = new (Exp ~> Exp) {
    def apply[T](e: Exp[T]): Exp[T] = {
      val subtrees = e.productIterator.toList map {
        case subExp: Exp[t] => transformer(subExp)
        case notExp => notExp
      }
      reflectiveCopy(e, subtrees: _*)
    }
  }

  def traverse(transformer: Exp ~> Exp): Exp ~> Exp = new (Exp ~> Exp) {
    def apply[T](e: Exp[T]): Exp[T] = {
      transformer(mapSubtrees(traverse(transformer))(e))
    }
  }
}
