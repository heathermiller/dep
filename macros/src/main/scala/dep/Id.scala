package ch.epfl.lamp.dep

import scala.reflect.macros.whitebox.Context


/** Type-level identity function. */
trait Id[X]
object Id {
  def apply[A](x: A): A = macro applyImpl
  def applyImpl(c: Context)(x: c.Tree): c.Tree = {
    import c.universe._
    q"$x"
  }
}
