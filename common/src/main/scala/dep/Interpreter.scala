package ch.epfl.lamp.dep

import scala.reflect.macros.whitebox.Context

import internal.{ MacroValueClass, TreeEvaluator }


/**
 * Generic interpreter type class.
 *
 * Given an instance `ip: Interpreter[A, X]` for some type-level
 * representation `X <: Pt[A]` of a term-level expression `x: A`,
 * `ip.value` returns `x`.
 *
 * Here's an illustrating example using Peano-encoded natural numbers:
 *
 * {{{
 *   class Zero extends Pt[Int]
 *   class Succ[N <: Pt[Int]] extends Pt[Int]
 *
 *   implicit def zeroInterpreter = Interpreter[Int, Zero](0)
 *   implicit def succInterpreter[N <: Pt[Int]](
 *       implicit pred: Interpreter[Int, N]) =
 *           Interpreter[Int, Succ[N]](pred.value + 1)
 * }}}
 */
final class Interpreter[+A, X] (val internal$value: A) {
  def value: A = macro MacroValueClass.unwrap
}

// FIXME: Coercion interpreters don't currently work as they cause
// implicit resolution to end up in an infinite loop.  This is likely
// caused by some issue related to Interpreter being covariant in its
// first argument.
object Interpreter /* extends CoercionInterpreters */ {

  /** Interpreter factory (macro) method. */
  def apply[A, X](value: A): Interpreter[A, X] =
    macro MacroValueClass.wrap1[Interpreter[A, X]]


  // Helper methods

  /**
   * Helper method for emitting interpreter definitions in implicit
   * materializers.
   */
  def fromTreeAndTypes(
    c: Context)(value: c.Tree, atp: c.Type, xtp: c.Type): c.Tree = {
    import c.universe._
    val ctor = appliedType(
      typeOf[Interpreter[_, _]].typeConstructor, List(atp, xtp))
    MacroValueClass.wrap(c)(ctor, value)
  }

  /**
   * Helper method for emitting interpreter definitions in implicit
   * materializers.
   */
  def fromTree[A: c.WeakTypeTag, X: c.WeakTypeTag](
    c: Context)(value: c.Tree): c.Tree = {
    import c.universe._
    val atp = implicitly[WeakTypeTag[A]].tpe
    val xtp = implicitly[WeakTypeTag[X]].tpe
    Interpreter.fromTreeAndTypes(c)(value, atp, xtp)
  }

  /**
   * Helper method for emitting interpreter definitions in implicit
   * materializers.
   */
  def fromExpr[A: c.WeakTypeTag, X: c.WeakTypeTag](
    c: Context)(value: c.Expr[A]): c.Tree = {
    import c.universe._
    Interpreter.fromTree[A, X](c)(q"${value.tree}")
  }

  /**
   * Helper method for emitting interpreter definitions in implicit
   * materializers.
   */
  def fromLiteral[A: c.WeakTypeTag, X: c.WeakTypeTag](
    c: Context)(value: A): c.Tree = {
    import c.universe._
    Interpreter.fromTree[A, X](c)(Literal(Constant(value)))
  }

  /**
   * Helper method for emitting interpreter definitions for unary
   * operations.
   *
   * Given an unary operation `op(x)` returning a value of literal
   * type and an interpreter `xip` for the arguments `x` of the
   * operation, this method will return a tree defining an interpreter
   * for the value `op(x)`.
   */
  def fromLiteralOp[A, B: c.WeakTypeTag, Y: c.WeakTypeTag](
    c: Context)(xip: c.Tree, op: A => B): c.Tree = {
    val x = TreeEvaluator.evalInterpreter[A](c)(xip)
    Interpreter.fromLiteral[B, Y](c)(op(x))
  }

  /**
   * Helper method for emitting interpreter definitions for binary
   * operations.
   *
   * Given a binary operation `op(x, y)` returning a value of literal
   * type and interpreters `xip` and `yip` for the arguments `x` and
   * `y` of the operation, this method will return a tree defining an
   * interpreter for the value `op(x, y)`.
   */
  def fromLiteralBinOp[A, B, C: c.WeakTypeTag, Z: c.WeakTypeTag](
    c: Context)(xip: c.Tree, yip: c.Tree, op: (A, B) => C): c.Tree = {
    val x = TreeEvaluator.evalInterpreter[A](c)(xip)
    val y = TreeEvaluator.evalInterpreter[B](c)(yip)
    Interpreter.fromLiteral[C, Z](c)(op(x, y))
  }
}


/**
 * Interpreters providing coercions between literal Scala types.
 */
trait CoercionInterpreters {

  /** As-Short coercion interpreter. */
  implicit def evalAsShort[A, X](
    implicit ip: Interpreter[A, X]): Interpreter[Short, X] =
    macro CoercionInterpreters.coerceImpl[A, Short, X]

  /** As-Int coercion interpreter. */
  implicit def evalAsInt[A, X](
    implicit ip: Interpreter[A, X]): Interpreter[Int, X] =
    macro CoercionInterpreters.coerceImpl[A, Int, X]

  /** As-Long coercion interpreter. */
  implicit def evalAsLong[A, X](
    implicit ip: Interpreter[A, X]): Interpreter[Long, X] =
    macro CoercionInterpreters.coerceImpl[A, Long, X]

  /** As-Float coercion interpreter. */
  implicit def evalAsFloat[A, X](
    implicit ip: Interpreter[A, X]): Interpreter[Float, X] =
    macro CoercionInterpreters.coerceImpl[A, Float, X]

  /** As-Double coercion interpreter. */
  implicit def evalAsDouble[A, X](
    implicit ip: Interpreter[A, X]): Interpreter[Double, X] =
    macro CoercionInterpreters.coerceImpl[A, Double, X]
}

object CoercionInterpreters {

  /** Generic coercion interpreter implementation. */
  def coerceImpl[A: c.WeakTypeTag, B: c.WeakTypeTag, X: c.WeakTypeTag](
    c: Context)(ip: c.Tree): c. Tree =
    Interpreter.fromLiteralOp[A, B, X](c)(ip, (x: A) => {

      import c.universe._

      // Big ugly explicit coercion function :-(
      val atp = implicitly[c.WeakTypeTag[A]].tpe
      val btp = implicitly[c.WeakTypeTag[B]].tpe
      val ret: Any = if (btp == typeOf[Short]) x match {
        case x: Byte  => x.toShort
        case _ =>
          c.abort(c.enclosingPosition, s"attempt to corece $atp to $btp")
      } else if (btp == typeOf[Int]) x match {
        case x: Byte  => x.toInt
        case x: Short => x.toInt
        case _ =>
          c.abort(c.enclosingPosition, s"attempt to corece $atp to $btp")
      } else if (btp == typeOf[Long]) x match {
        case x: Byte  => x.toLong
        case x: Short => x.toLong
        case x: Int   => x.toLong
        case _ =>
          c.abort(c.enclosingPosition, s"attempt to corece $atp to $btp")
      } else if (btp == typeOf[Float]) x match {
        case x: Byte  => x.toFloat
        case x: Short => x.toFloat
        case x: Int   => x.toFloat
        case x: Long  => x.toFloat
        case _ =>
          c.abort(c.enclosingPosition, s"attempt to corece $atp to $btp")
      } else if (btp == typeOf[Double]) x match {
        case x: Byte  => x.toDouble
        case x: Short => x.toDouble
        case x: Int   => x.toDouble
        case x: Long  => x.toDouble
        case x: Float => x.toDouble
        case _ =>
          c.abort(c.enclosingPosition, s"attempt to corece $atp to $btp")
      } else
        c.abort(c.enclosingPosition, s"attempt to corece $atp to $btp")
      ret.asInstanceOf[B]
    })
}
