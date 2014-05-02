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

  /** Implicit materializer for type-level integer literals. */
  implicit def evalInt[X <: Cst[Int]]: Interpreter[Int, X] =
    macro Literals.evalImpl[Int, X]

  /** Implicit materializer for type-level Boolean literals. */
  implicit
  def evalBoolean[X <: Cst[Boolean]]: Interpreter[Boolean, X] =
    macro Literals.evalImpl[Boolean, X]
}


/** Macro bundle for materializing type-level literals. */
class Literals(val c: Context) {

  /** Materializer macro implementation for type-level literals. */
  def evalImpl[A: c.WeakTypeTag, X <: Pt[A]: c.WeakTypeTag] = {
    import c.universe._
    val atp = implicitly[WeakTypeTag[A]].tpe.dealias
    val xtp = implicitly[WeakTypeTag[X]].tpe.dealias
    val tps = atp :: xtp :: (xtp.typeArgs map (_.dealias))
    val annotss = tps collect {
      case AnnotatedType(as, _) => as map (_.tree)
    }
    val x = annotss.flatten collectFirst {
      case q"new $t($x)" if t.tpe == typeOf[lit] => x
    }
    x match {
      case Some(x) => Interpreter.fromTree[A, X](c)(x)
      case None => {
        val tpsStr = tps.mkString("'", "', '", "'")
        c.abort(c.enclosingPosition,
          s"neither of the types $tpsStr have @lit annotations.")
      }
    }
  }
}
