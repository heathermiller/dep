package ch.epfl.lamp.dep

import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.TypecheckException
import scala.reflect.runtime.{ universe => ru }

import internal.TypeEvaluator


/**
 * Generic interpreter type class.
 *
 * Given an instance `xAsA: X :~ A` for some type-level representation
 * `X` of a term-level expression `x: A`, `xAsA.value` returns `x`.
 *
 * Here's an illustrating example using Peano-encoded natural numbers:
 *
 * {{{
 *   class Zero extends Pt[Int]
 *   class Succ[N <: Pt[Int]] extends Pt[Int]
 *
 *   implicit def zeroInterpreter = :~[Zero, Int](0)
 *   implicit def succInterpreter[N](implicit pred: :~[N, Int]) =
 *       :~[Succ[N], Int](pred.value + 1)
 * }}}
 */
final class :~[X, A] (x: => A) { lazy val value = x }
object :~ {

  /** Interpreter factory (macro) method. */
  def apply[X, A](x: => A): :~[X, A] = new :~[X, A](x)

  /** Generic implicit materializer */
  implicit def xAsA[X, A]: X :~ A = macro InterpreterMaterializer.impl[X, A]
}


/** Macro bundle for materializing interpreters. */
class InterpreterMaterializer(val c: Context) extends TypeEvaluator {

  import c.universe._

  /** Materializer macro implementation for interpreters. */
  def impl[X: c.WeakTypeTag, A: c.WeakTypeTag]: c.Tree = {

    val xtp = implicitly[c.WeakTypeTag[X]].tpe
    val atp = implicitly[c.WeakTypeTag[A]].tpe
    val tptp = typeOf[ru.Type]
    val typed = try {

      // Find the term-level expression corresponding to `X`.
      val tree =
        if (atp <:< tptp) q"scala.reflect.runtime.universe.typeOf[$xtp]"
        else typeToTerm(xtp)

      // Type-check the expression and compare its type against `A`.
      val typed = c.typecheck(tree)
      if (!(typed.tpe <:< atp))
        error(s"expected $atp, but found ${typed.tpe}")
      typed

    } catch {
      case TypecheckException(pos, msg) =>
        val pos2 = pos.asInstanceOf[c.Position]
        c.error(pos2, msg)
        c.abort(pos2, msg)
    }

    // Emit the interpreter instantiation.
    val ctor = appliedType(typeOf[:~[_, _]].typeConstructor, List(xtp, atp))
    q"new $ctor($typed)"
  }


  // Helper methods

  /** Build and and throw a [[TypecheckException]]. */
  def error(msg: String): Nothing = throw new TypecheckException(
    c.enclosingPosition, "interpretation error: " + msg)
}
