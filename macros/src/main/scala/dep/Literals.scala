package ch.epfl.lamp.dep

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context


/**
 * Annotation class for type-level literals.
 *
 * Parameters of the [[Pt]] type annotated with instances of this
 * class represent type-level literals.
 *
 * Use case:
 * {{{
 *   import ch.epfl.lamp.dep._
 *
 *   interpret[Boolean, Cst[Boolean @lit(true)]]  // returns `true`
 *   prove[Cst[Boolean@lit(true)] === True]       // succeeds
 *   prove[Cst[Boolean@lit(true)] === False]      // fails
 * }}}
 */
final class lit(x: Any) extends StaticAnnotation


/** Macro bundle for materializing type-level literals. */
class Literals(val c: Context) {

  /** Materializer macro implementation for type-level literals. */
  def evalImpl[A: c.WeakTypeTag, X <: Pt[A]: c.WeakTypeTag] = {
    import c.universe._
    val xtp = implicitly[WeakTypeTag[X]].tpe.dealias
    val ctp = xtp.typeConstructor
    if (ctp != typeOf[Pt[_]].typeConstructor)
      c.abort(c.enclosingPosition, s"not a literal container: $ctp")
    val (atp, annots) = xtp.typeArgs match {
      case AnnotatedType(annots, atp) :: Nil => (atp, annots)
      case _ =>
        c.abort(c.enclosingPosition, s"not @lit-annotated: $xtp")
    }
    val ox = annots map (_.tree) collectFirst {
      case q"new $t($x)" if t.tpe == typeOf[lit] => x
    }
    ox match {
      case Some(x) => Interpreter.fromTreeAndTypes(c)(x, atp, xtp)
      case None =>
        c.abort(c.enclosingPosition, s"not a type-level literal: $xtp")
    }
  }
}
