package traversal

import scala.language.postfixOps

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
