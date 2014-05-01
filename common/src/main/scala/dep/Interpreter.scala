package ch.epfl.lamp.dep

import scala.reflect.macros.whitebox.Context

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
final class Interpreter[+A, X <: Pt[A]] private (val internalValue: A) {
  def value: A = macro Interpreter.valueImpl[A, X]
}

object Interpreter {

  def apply[A, X <: Pt[A]](x: A) = new Interpreter[A, X](x)

  def valueImpl[A: c.WeakTypeTag, X <: Pt[A]: c.WeakTypeTag](
    c: Context) = {

    import c.universe._

    val owner = typeOf[Interpreter.type].typeSymbol
    val p = c.prefix.tree
    val default = q"$p.internalValue"
    val ret = p match {
      case Apply(fun, args) =>
        if (fun.symbol.owner.asType == owner) args.head
        else default
      case _ => default
    }
    c.Expr[A](ret)
  }

  def fromTree[A: c.WeakTypeTag, X <: Pt[A]: c.WeakTypeTag](
    c: Context)(value: c.Tree): c.Expr[Interpreter[A, X]] = {
    import c.universe._
    val atp = implicitly[WeakTypeTag[A]].tpe
    val xtp = implicitly[WeakTypeTag[X]].tpe
    val comp = typeOf[Interpreter[_, _]].typeSymbol.companion
    c.Expr(q"$comp.apply[$atp, $xtp]($value)")
  }

  def fromExpr[A: c.WeakTypeTag, X <: Pt[A]: c.WeakTypeTag](
    c: Context)(value: c.Expr[A]): c.Expr[Interpreter[A, X]] = {
    import c.universe._
    Interpreter.fromTree[A, X](c)(q"${value.tree}")
  }

  def fromLiteral[A: c.WeakTypeTag, X <: Pt[A]: c.WeakTypeTag](
    c: Context)(value: A): c.Expr[Interpreter[A, X]] = {
    import c.universe._
    Interpreter.fromTree[A, X](c)(Literal(Constant(value)))
  }
}
