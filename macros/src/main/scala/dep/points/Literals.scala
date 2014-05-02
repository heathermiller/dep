package ch.epfl.lamp.dep
package points

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

/**
 * Annotation class for type-level literals.
 *
 * Parameters of the [[Cst]] type annotated with instances of this
 * class represent type-level literals.
 *
 * Use case:
 * {{{
 *   import ch.epfl.lamp.dep._
 *   import points._
 *
 *   interpret[Boolean, Cst[Boolean @lit(true)]]  // returns `true`
 *   prove[Cst[Boolean@lit(true)] === True]       // succeeds
 *   prove[Cst[Boolean@lit(true)] === False]      // fails
 * }}}
 */
final class lit(x: Any) extends StaticAnnotation

/** Type-level constant container trait. */
sealed trait Cst[+A] extends Pt[A]
object Cst {

  /** Implicit materializer for type-level literals. */
  implicit def evalInt[A, X <: Cst[A]]: Interpreter[A, X] =
    macro Literals.evalImpl[A, X]
}


/** Macro bundle for materializing type-level literals. */
class Literals(val c: Context) {

  /** Materializer macro implementation for type-level literals. */
  def evalImpl[A: c.WeakTypeTag, X <: Pt[A]: c.WeakTypeTag] = {
    import c.universe._
    val xtp = implicitly[WeakTypeTag[X]].tpe.dealias
    val ctp = xtp.typeConstructor
    if (ctp != typeOf[Cst[_]].typeConstructor)
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
