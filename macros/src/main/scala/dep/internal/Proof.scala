package ch.epfl.lamp.dep
package internal

import scala.reflect.macros.whitebox.Context

/**
 * Proof base trait.
 *
 * This is the base trait for implicit values witnessing the truth of
 * a type-level proposition `X <: Pt[Boolean]`.
 */
@scala.annotation.implicitNotFound("could not prove ${X}")
trait Proof[X <: Pt[Boolean]]

/** Companion of the `Proof` trait. */
object Proof {

  /** Implicit materializer for proofs. */
  implicit def materializer[X <: Pt[Boolean]](
    implicit ip: Interpreter[Boolean, X]): Proof[X] =
    macro Materializer.impl[X]

  /** Implicit materializer macro bundle for proofs. */
  class Materializer(val c: Context) {

    import c.universe._

    /** Implicit materializer implementation. */
    def impl[X <: Pt[Boolean]: c.WeakTypeTag](ip: c.Tree) = {

      // Get the type of the type-level proposition and construct the
      // corresponding proof type.
      val tp = implicitly[c.WeakTypeTag[X]].tpe

      // Evaluate the proposition using the implicit interpreter.
      val prop = TreeEvaluator.evalInterpreter[Boolean](c)(ip)
      println(s"Compile time value of proposition $tp: $prop")

      // Materialize the implicit evidence value if and only if the
      // proposition is true.
      if (prop) {
        // Construct and return the proof type.
        val ctr = typeOf[Proof[points.True]].typeConstructor
        val ptp = appliedType(ctr, List(tp))
        c.Expr[Proof[X]](q"new $ptp { }")
      } else c.abort(c.enclosingPosition, s"${tp} evaluated to false.")
    }
  }
}
