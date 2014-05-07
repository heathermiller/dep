package ch.epfl.lamp.dep
package internal

import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.TypecheckException

/**
 * Proof base trait.
 *
 * This is the base trait for implicit values witnessing the truth of
 * a type-level proposition `X :~ Boolean`.
 */
@scala.annotation.implicitNotFound("could not prove ${X}")
final class Proof[X]

/** Companion of the `Proof` trait. */
object Proof {

  /** Implicit materializer for proofs. */
  implicit def materializer[X]: Proof[X] = macro ProofMaterializer.impl[X]
}

/** Implicit materializer macro bundle for proofs. */
class ProofMaterializer(val c: Context)
    extends TypeEvaluator with TreeEvaluator {

  import c.universe._

  /** Implicit materializer implementation. */
  def impl[X: c.WeakTypeTag]: c.Tree = {

    val xtp = implicitly[c.WeakTypeTag[X]].tpe
    val typed = try {

      // Find the term-level expression corresponding to `X`.
      val tree = typeToTerm(xtp)

      // Type-check the expression and make sure it is of type
      // Boolean.
      val typed = c.typecheck(tree)
      if (!(typed.tpe <:< typeOf[Boolean]))
        error("type-level propositions must have type Boolean, "
          + "found " + typed.tpe)
      typed

    } catch {
      case TypecheckException(pos, msg) =>
        val pos2 = pos.asInstanceOf[c.Position]
        c.abort(pos2, msg)
    }

    // Evaluate the proposition using the implicit interpreter.
    val prop = eval[Boolean](typed)
    println(s"Compile time value of proposition $typed: $prop")

    // Materialize the implicit evidence value if and only if the
    // proposition is true.
    if (prop) {
      // Construct and return the proof type.
      val ctor = appliedType(typeOf[Proof[_]].typeConstructor, List(xtp))
      q"new $ctor"
    } else c.abort(c.enclosingPosition, s"${xtp} evaluated to false.")
  }


  // Helpers

  /** Build and and throw a [[TypecheckException]]. */
  def error(msg: String): Nothing = throw new TypecheckException(
    c.enclosingPosition, "proof error: " + msg)
}
