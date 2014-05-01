package ch.epfl.lamp.dep
package internal

import scala.reflect.macros.whitebox.Context


/** Implementations of top-level package macros. */
class PackageMacros(val c: Context) {

  import c.universe._

  /**
   * Interpret a type-level expression.
   *
   * Given a type-level representation `X <: Pt[A]` of a term-level
   * expression `x: A`, `interpret[A, X]` returns `x`.
   */
  def interpretImpl[A: c.WeakTypeTag](ip: c.Tree) = q"$ip.value"

  /**
   * Prove a type-level proposition.
   *
   * Given a type-level representation `X <: Pt[Boolean]` of a
   * term-level proposition `x: Boolean`, `prove[X]` returns `()` if
   * `x == true` or produces a compile-time error otherwise.
   */
  def proveImpl[X <: Pt[Boolean]: c.WeakTypeTag](pf: c.Tree) = q"()"
}
