package ch.epfl.lamp.dep

import scala.reflect.macros.blackbox.Context

import internal.Proof


/**
 * Type-level equivalence base trait.
 *
 * This is the base trait of implicit values witnessing the
 * equivalence of any two any type-level expressions.  A pair of
 * type-level expressions `X` and `Y` are considered equivalent of
 * there is an instance of `X =*= Y`.
 */
trait =*=[X, Y]
object =*= {

  /** Implicit materializer for point-equality. */
  implicit def ptEquiv[X, Y](
    implicit eqPf: Proof[X == Y]): =*=[X, Y] = macro ptEquivImpl[X, Y]
  def ptEquivImpl[X: c.WeakTypeTag, Y: c.WeakTypeTag](
    c: Context)(eqPf: c.Tree) = {
    import c.universe._
    val xtp = implicitly[WeakTypeTag[X]].tpe
    val ytp = implicitly[WeakTypeTag[Y]].tpe
    val ctor = appliedType(typeOf[=*=[_, _]].typeConstructor, List(xtp, ytp))
    q"new $ctor { }"
  }

  /** Implicit materializer for reflexivity. */
  implicit def reflexiveEquiv[X]: =*=[X, X] = macro reflexiveEquivImpl[X]
  def reflexiveEquivImpl[X: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val xtp = implicitly[WeakTypeTag[X]].tpe
    val ctor = appliedType(typeOf[=*=[_, _]].typeConstructor, List(xtp, xtp))
    q"new $ctor { }"
  }
}
