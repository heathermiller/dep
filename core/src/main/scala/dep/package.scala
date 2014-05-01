package ch.epfl.lamp

import dep.internal.Proof


/** Package object containing top-level definitions. */
package object dep {

  /**
   * Interpret a type-level expression.
   *
   * Given a type-level representation `X <: Pt[A]` of a term-level
   * expression `x: A`, `interpret[A, X]` returns `x`.
   */
  def interpret[A, X <: Pt[A]](implicit ip: Interpreter[A, X]): A = ip.value

  /**
   * Prove a type-level proposition.
   *
   * Given a type-level representation `X <: Pt[Boolean]` of a
   * term-level proposition `x: Boolean`, `prove[X]` returns `()` if
   * `x == true` or produces a compile-time error otherwise.
   */
  def prove[X <: Pt[Boolean]](implicit pf: Proof[X]): Unit = ()
}
