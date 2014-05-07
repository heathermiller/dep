package ch.epfl.lamp.dep
package internal

import scala.reflect.macros.blackbox.Context


/** Implementations of top-level package macros. */
class PackageMacros(val c: Context) {

  import c.universe._

  /**
   * Interpret a type-level expression.
   *
   * Given a type-level representation `X :~ A` of a term-level
   * expression `x: A`, `interpret[A, X]` returns `x`.
   */
  def interpretImpl(xAsA: c.Tree): c.Tree = q"$xAsA.value"

  /**
   * Prove a type-level proposition.
   *
   * Given a type-level representation `X :~ Boolean` of a term-level
   * proposition `x: Boolean`, `prove[X]` returns `()` if `x == true`
   * or produces a compile-time error otherwise.
   */
  def proveImpl(pf: c.Tree): c.Tree = q"()"
}
