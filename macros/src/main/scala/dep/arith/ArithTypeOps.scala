package ch.epfl.lamp.dep
package arith

import scala.reflect.macros.whitebox.Context

import internal.TreeEvaluator


// FIXME: Remove all this boiler plate.  Generate these traits and
// their interpreters using a macro annotation.

// Peano literals.

/** Type-level zero literal. */
sealed trait Zero extends Pt[Int]
object Zero {
  implicit def eval: Interpreter[Int, Zero] = macro evalImpl
  def evalImpl(c: Context) = Interpreter.fromLiteral[Int, Zero](c)(0)
}

/** Type-level successor operator. */
sealed trait Succ[N <: Pt[Int]] extends Pt[Int]
object Succ {
  implicit def eval[N <: Pt[Int]](
    implicit pred: Interpreter[Int, N]): Interpreter[Int, Succ[N]] =
    macro evalImpl[N]
  def evalImpl[N <: Pt[Int]: c.WeakTypeTag](c: Context)(pred: c.Tree) = {
    import c.universe._
    Interpreter.fromLiteral[Int, Succ[N]](c)(
      TreeEvaluator.evalInterpreter[Int](c)(pred) + 1)
  }
}


// Arithmetic operators.

/** Type-level integer addition. */
sealed trait +[M <: Pt[Int], N <: Pt[Int]] extends Pt[Int]
object + {
  implicit def eval[M <: Pt[Int], N <: Pt[Int]](
    implicit mip: Interpreter[Int, M], nip: Interpreter[Int, N])
      : Interpreter[Int, M + N] = macro evalImpl[M, N]
  def evalImpl[M <: Pt[Int]: c.WeakTypeTag, N <: Pt[Int]: c.WeakTypeTag](
    c: Context)(mip: c.Tree, nip: c.Tree) = {
    import c.universe._
    val m = TreeEvaluator.evalInterpreter[Int](c)(mip)
    val n = TreeEvaluator.evalInterpreter[Int](c)(nip)
    Interpreter.fromLiteral[Int, M + N](c)(m + n)
  }
}

/** Type-level integer subtraction. */
sealed trait -[M <: Pt[Int], N <: Pt[Int]] extends Pt[Int]
object - {
  implicit def eval[M <: Pt[Int], N <: Pt[Int]](
    implicit mip: Interpreter[Int, M], nip: Interpreter[Int, N])
      : Interpreter[Int, M - N] = macro evalImpl[M, N]
  def evalImpl[M <: Pt[Int]: c.WeakTypeTag, N <: Pt[Int]: c.WeakTypeTag](
    c: Context)(mip: c.Tree, nip: c.Tree) = {
    import c.universe._
    val m = TreeEvaluator.evalInterpreter[Int](c)(mip)
    val n = TreeEvaluator.evalInterpreter[Int](c)(nip)
    Interpreter.fromLiteral[Int, M - N](c)(m - n)
  }
}

/** Type-level integer multiplication. */
sealed trait *[M <: Pt[Int], N <: Pt[Int]] extends Pt[Int]
object * {
  implicit def eval[M <: Pt[Int], N <: Pt[Int]](
    implicit mip: Interpreter[Int, M], nip: Interpreter[Int, N])
      : Interpreter[Int, *[M, N]] = macro evalImpl[M, N]
  def evalImpl[M <: Pt[Int]: c.WeakTypeTag, N <: Pt[Int]: c.WeakTypeTag](
    c: Context)(mip: c.Tree, nip: c.Tree) = {
    import c.universe._
    val m = TreeEvaluator.evalInterpreter[Int](c)(mip)
    val n = TreeEvaluator.evalInterpreter[Int](c)(nip)
    Interpreter.fromLiteral[Int, *[M, N]](c)(m * n)
  }
}

/** Type-level integer division. */
sealed trait /[M <: Pt[Int], N <: Pt[Int]] extends Pt[Int]
object / {
  implicit def eval[M <: Pt[Int], N <: Pt[Int]](
    implicit mip: Interpreter[Int, M], nip: Interpreter[Int, N])
      : Interpreter[Int, M / N] = macro evalImpl[M, N]
  def evalImpl[M <: Pt[Int]: c.WeakTypeTag, N <: Pt[Int]: c.WeakTypeTag](
    c: Context)(mip: c.Tree, nip: c.Tree) = {
    import c.universe._
    val m = TreeEvaluator.evalInterpreter[Int](c)(mip)
    val n = TreeEvaluator.evalInterpreter[Int](c)(nip)
    Interpreter.fromLiteral[Int, M / N](c)(m / n)
  }
}
