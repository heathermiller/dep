package ch.epfl.lamp.dep
package points

/** Type-level truth literal. */
sealed trait True extends Pt[Boolean]
object True {
  implicit object Eval extends Interpreter[Boolean, True] {
    val value = true
  }
}

/** Type-level falsehood literal. */
sealed trait False extends Pt[Boolean]
object False {
  implicit object Eval extends Interpreter[Boolean, False] {
    val value = false
  }
}

/** Type-level logic negation. */
trait ![X <: Pt[Boolean]] <: Pt[Boolean]
object ! {
  implicit def eval[X <: Pt[Boolean]](
    implicit xip: Interpreter[Boolean, X]) =
    new Interpreter[Boolean, ![X]] { val value = !xip.value }
}

/** Type-level logic conjunction. */
trait &&[X <: Pt[Boolean], Y <: Pt[Boolean]] <: Pt[Boolean]
object && {
  implicit def eval[X <: Pt[Boolean], Y <: Pt[Boolean]](
    implicit xip: Interpreter[Boolean, X], yip: Interpreter[Boolean, Y]) =
    new Interpreter[Boolean, X && Y] { val value = xip.value && yip.value }
}

/** Type-level logic disjunction. */
trait ||[X <: Pt[Boolean], Y <: Pt[Boolean]] <: Pt[Boolean]
object || {
  implicit def eval[X <: Pt[Boolean], Y <: Pt[Boolean]](
    implicit xip: Interpreter[Boolean, X], yip: Interpreter[Boolean, Y]) =
    new Interpreter[Boolean, X || Y] { val value = xip.value || yip.value }
}

/** Type-level equality operator. */
trait ===[X <: Pt[Any], Y <: Pt[Any]] <: Pt[Boolean]
object === {
  implicit def eval[X <: Pt[Any], Y <: Pt[Any]](
    implicit xip: Interpreter[Any, X], yip: Interpreter[Any, Y]) =
    new Interpreter[Boolean, X === Y] { val value = xip.value == yip.value }
}
