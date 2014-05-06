package ch.epfl.lamp.dep

import scala.reflect.macros.whitebox.Context

import internal.TreeEvaluator


// FIXME: Remove all this boiler plate.  Generate these traits and
// their interpreters using a macro annotation.


// Type-level Boolean literals.

/** Type-level truth literal. */
sealed trait True extends Pt[Boolean]
object True {
  implicit def eval: Interpreter[Boolean, True] = macro evalImpl
  def evalImpl(c: Context) = Interpreter.fromLiteral[Boolean, True](c)(true)
}

/** Type-level falsehood literal. */
sealed trait False extends Pt[Boolean]
object False {
  implicit def eval: Interpreter[Boolean, False] = macro evalImpl
  def evalImpl(c: Context) = Interpreter.fromLiteral[Boolean, False](c)(false)
}


// Type-level logic operations.

/** Type-level logic negation. */
trait ![X <: Pt[Boolean]] <: Pt[Boolean]
object ! {
  implicit def eval[X <: Pt[Boolean]](
    implicit xip: Interpreter[Boolean, X]): Interpreter[Boolean, ![X]] =
    macro evalImpl[X]
  def evalImpl[X <: Pt[Boolean]: c.WeakTypeTag](c: Context)(xip: c.Tree) =
    Interpreter.fromLiteralOp[Boolean, Boolean, ![X]](c)(xip, !_)
}

/** Type-level logic conjunction. */
trait &&[X <: Pt[Boolean], Y <: Pt[Boolean]] <: Pt[Boolean]
object && {
  implicit def eval[X <: Pt[Boolean], Y <: Pt[Boolean]](
    implicit xip: Interpreter[Boolean, X], yip: Interpreter[Boolean, Y])
      : Interpreter[Boolean, X && Y] = macro evalImpl[X, Y]
  def evalImpl[X <: Pt[Boolean]: c.WeakTypeTag,
    Y <: Pt[Boolean]: c.WeakTypeTag](c: Context)(xip: c.Tree, yip: c.Tree) =
    Interpreter.fromLiteralBinOp[Boolean, Boolean, Boolean, X && Y](c)(
      xip, yip, _ && _)
}

/** Type-level logic disjunction. */
trait ||[X <: Pt[Boolean], Y <: Pt[Boolean]] <: Pt[Boolean]
object || {
  implicit def eval[X <: Pt[Boolean], Y <: Pt[Boolean]](
    implicit xip: Interpreter[Boolean, X], yip: Interpreter[Boolean, Y])
      : Interpreter[Boolean, X || Y] = macro evalImpl[X, Y]
  def evalImpl[X <: Pt[Boolean]: c.WeakTypeTag,
    Y <: Pt[Boolean]: c.WeakTypeTag](c: Context)(xip: c.Tree, yip: c.Tree) =
    Interpreter.fromLiteralBinOp[Boolean, Boolean, Boolean, X || Y](c)(
      xip, yip, _ || _)
}


// Comparisons

/** Type-level equality operator. */
trait ==[X, Y] <: Pt[Boolean]
object == {
  implicit def eval[X, Y](
    implicit xip: Interpreter[Any, X], yip: Interpreter[Any, Y])
      : Interpreter[Boolean, X == Y] = macro evalImpl[X, Y]
  def evalImpl[X: c.WeakTypeTag, Y: c.WeakTypeTag](
    c: Context)(xip: c.Tree, yip: c.Tree) =
    Interpreter.fromLiteralBinOp[Any, Any, Boolean, X == Y](c)(
      xip, yip, _ == _)
}

