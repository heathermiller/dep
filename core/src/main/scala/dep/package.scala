package ch.epfl.lamp

import scala.language.experimental.macros

import dep.internal.{ PackageMacros, Proof }


/** Package object containing top-level definitions. */
package object dep {

  /**
   * Interpret a type-level expression.
   *
   * Given a type-level representation `X <: Pt[A]` of a term-level
   * expression `x: A`, `interpret[A, X]` returns `x`.
   */
  def interpret[A, X](implicit ip: Interpreter[A, X]): A =
    macro PackageMacros.interpretImpl[A]

  /**
   * Prove a type-level proposition.
   *
   * Given a type-level representation `X <: Pt[Boolean]` of a
   * term-level proposition `x: Boolean`, `prove[X]` returns `()` if
   * `x == true` or produces a compile-time error otherwise.
   */
  def prove[X <: Pt[Boolean]](implicit pf: Proof[X]): Unit =
    macro PackageMacros.proveImpl[X]

  /**
   * Implicit materializer for type-level literals.
   *
   * Parameters of the [[Pt]] type annotated with the `@lit`
   * annotation represent type-level literals.
   *
   * Use case:
   * {{{
   *   import ch.epfl.lamp.dep._
   *
   *   interpret[Boolean, Pt[Boolean @lit(true)]]  // returns `true`
   *   prove[Pt[Boolean@lit(true)] === True]       // succeeds
   *   prove[Pt[Boolean@lit(true)] === False]      // fails
   * }}}
   */
  implicit def evalLit[A, X <: Pt[A]]: Interpreter[A, X] =
    macro Literals.evalImpl[A, X]
}
