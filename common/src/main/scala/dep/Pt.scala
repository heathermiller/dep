package ch.epfl.lamp.dep

/**
 * Point base trait.
 *
 * This is the base trait of any type-level expression.  Term-level
 * expressions `x: A` are represented by point types `X <: Pt[A]`.
 */
trait Pt[+T]
