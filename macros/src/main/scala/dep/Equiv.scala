package ch.epfl.lamp.dep

import scala.reflect.macros.whitebox.Context

import internal.Proof
import points.===


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
  implicit def ptEquiv[X <: Pt[Any], Y <: Pt[Any]](
    implicit eqPf: Proof[X === Y]): =*=[X, Y] = macro ptEquivImpl[X, Y]
  def ptEquivImpl[X <: Pt[Any]: c.WeakTypeTag, Y <: Pt[Any]: c.WeakTypeTag](
    c: Context)(eqPf: c.Tree) = {
    import c.universe._
    val ctor = typeOf[=*=[_, _]].typeConstructor
    val xtp = implicitly[WeakTypeTag[X]].tpe
    val ytp = implicitly[WeakTypeTag[Y]].tpe
    q"new $ctor[$xtp, $ytp] {}"
  }

  /** Implicit materializer for reflexivity. */
  implicit def reflexiveEquiv[X]: =*=[X, X] = macro reflexiveEquivImpl[X]
  def reflexiveEquivImpl[X: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val ctor = typeOf[=*=[_, _]].typeConstructor
    val xtp = implicitly[WeakTypeTag[X]].tpe
    q"new $ctor[$xtp, $xtp] {}"
  }
}
