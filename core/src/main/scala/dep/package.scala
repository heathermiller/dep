package ch.epfl.lamp

import scala.language.experimental.macros

import dep.internal.{ PackageMacros, Proof }


/** Package object containing top-level definitions. */
package object dep {

  /**
   * Interpret a type-level expression.
   *
   * Given a type-level representation `X :~ A` of a term-level
   * expression `x: A`, `interpret[A, X]` returns `x`.
   */
  def interpret[A, X](implicit xAsA: X :~ A): A =
    macro PackageMacros.interpretImpl

  /**
   * Prove a type-level proposition.
   *
   * Given a type-level representation `X :~ Boolean` of a term-level
   * proposition `x: Boolean`, `prove[X]` returns `()` if `x == true`
   * or produces a compile-time error otherwise.
   */
  def prove[X](implicit pf: Proof[X]): Unit =
    macro PackageMacros.proveImpl
}
