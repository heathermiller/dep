package ch.epfl.lamp.dep

/**
 * Generic interpreter type class.
 *
 * Given an instance `ip: Interpreter[A, X]` for some type-level
 * representation `X <: Pt[A]` of a term-level expression `x: A`,
 * `v.value` returns `x`.
 *
 * Here's an illustrating example using Peano-encoded natural numbers:
 *
 * {{{
 *   class Zero extends Pt[Int]
 *   class Succ[N <: Pt[Int]] extends Pt[Int]
 *
 *   implicit object ZeroInterpreter extends Interpreter[Int, Zero] {
 *       val value = 0
 *   }
 *   implicit def succInterpreter[N <: Pt[Int]](
 *       implicit pred: Interpreter[Int, N]): Interpreter[Int, Succ[N]] =
 *           new Interpreter[Int, Succ[N]] { val value = pred.value + 1 }
 * }}}
 */
trait Interpreter[+A, X <: Pt[A]] { val value: A }
