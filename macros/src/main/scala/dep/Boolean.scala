package ch.epfl.lamp.dep

import scala.reflect.macros.whitebox.Context


// Type-level Boolean literals.

/** Type-level truth literal. */
sealed trait True
object True {
  def apply: Boolean = macro applyImpl
  def applyImpl(c: Context) = { import c.universe._; q"true" }
}

/** Type-level falsehood literal. */
sealed trait False
object False {
  def apply: Boolean = macro applyImpl
  def applyImpl(c: Context) = { import c.universe._; q"false" }
}


// Type-level logic operations.

/** Type-level logic negation. */
trait ![X]
object ! {
  def apply(x: Boolean): Boolean = macro applyImpl
  def applyImpl(c: Context)(x: c.Tree): c.Tree = {
    import c.universe._
    x match {
      case Literal(Constant(x: Boolean)) => Literal(Constant(!x))
      case _ => q"!$x"
    }
  }
}

/** Type-level logic conjunction. */
trait &&[X, Y]
object && {
  def apply(x: Boolean, y: Boolean): Boolean = macro applyImpl
  def applyImpl(c: Context)(x: c.Tree, y: c.Tree) = {
    import c.universe._
    (x, y) match {
      case (Literal(Constant(x: Boolean)), Literal(Constant(y: Boolean))) =>
        Literal(Constant(x && y))
      case _ => q"$x && y"
    }
  }
}

/** Type-level logic disjunction. */
trait ||[X, Y]
object || {
  def apply(x: Boolean, y: Boolean): Boolean = macro applyImpl
  def applyImpl(c: Context)(x: c.Tree, y: c.Tree) = {
    import c.universe._
    (x, y) match {
      case (Literal(Constant(x: Boolean)), Literal(Constant(y: Boolean))) =>
        Literal(Constant(x || y))
      case _ => q"$x || y"
    }
  }
}


// Comparisons

/** Type-level equality operator. */
trait ==[X, Y]
object == {
  def apply(x: Any, y: Any): Boolean = macro applyImpl
  def applyImpl(c: Context)(x: c.Tree, y: c.Tree) = {
    import c.universe._
    (x, y) match {
      case (Literal(Constant(x)), Literal(Constant(y))) =>
        Literal(Constant(x == y))
      case _ => q"$x == y"
    }
  }
}
