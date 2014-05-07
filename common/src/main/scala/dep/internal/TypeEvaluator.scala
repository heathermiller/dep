package ch.epfl.lamp.dep
package internal

import scala.annotation.StaticAnnotation

import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.TypecheckException


/**
 * Bundle for evaluating type-level expressions to runtime values.
 */
trait TypeEvaluator {

  val c: Context

  import c.universe._

  /**
   * Convert a type-level expression to the corresponding term-level
   * expression.
   */
  def typeToTerm(tpe: Type): Tree = {

    // Helper function to identify function type symbols.
    def isFunction(sym: Symbol): Boolean = {
      val scalaPkg = typeOf[Any].typeSymbol.owner
      val name = sym.name.toString
      if ((sym.owner == scalaPkg) && (name.take(8) == "Function")) {
        try {
          val suffix = name.drop(8).toInt
          suffix > 0
        } catch {
          case _: java.lang.NumberFormatException => false
        }
      } else false
    }

    val dealiased = tpe.dealias

    // Check for literal annotations first.
    val annots = dealiased match {
      case AnnotatedType(annots, t) => annots map (_.tree) collect {
        case q"new $a($x)" => (a, x, t)
      }
      case _ => Nil
    }

    if (annots.isEmpty) {
      val ctor = dealiased.typeConstructor
      val ctorSym = ctor.typeSymbol
      val args = dealiased.typeArgs

      if (ctor == typeOf[:~[_, _]].typeConstructor) args match {
        // Type ascription
        case x :: a :: Nil => q"${typeToTerm(x)}: $a"
      } else if (ctor == typeOf[:=[_, _]].typeConstructor) args match {
        // Variable binding
        case x :: b :: Nil => typeToTerm(x) match {
          case Typed(Ident(n), t) =>
            q"val ${n.toTermName}: $t = ${typeToTerm(b)}"
          case _ => error(s"malformed variable definition: $dealiased")
        }
      } else if (ctor == typeOf[in[_, _]].typeConstructor) args match {
        // Type ascription
        case x :: y :: Nil => q"${typeToTerm(x)}; ${typeToTerm(y)}"
      } else if (isFunction(ctorSym)) {
        // Function abstraction
        val as = args.dropRight(1) map typeToTerm map {
          case a @ Typed(Ident(n), t) => q"val ${n.toTermName}: $t"
          case a => error(s"malformed parameter expression: $a")
        }
        q"(..$as) => ${typeToTerm(args.last)}"
      } else if (ctor == typeOf[@@[_, _]].typeConstructor) args match {
        // Function application
        case x :: y:: Nil  => q"${typeToTerm(x)}.apply(${typeToTerm(y)})"
      } else {
        // Type-level operator
        val objSym = if (ctorSym.isModule) ctorSym else ctorSym.companion
        if (objSym == NoSymbol)
          error(s"type-level operator $tpe is missing a companion object")
        if (args.isEmpty) q"$objSym.apply"
        else q"$objSym.apply(..${args map typeToTerm})"
      }
    } else annots match {
      case (a, x, _) :: Nil if a.tpe == typeOf[lit] => x
      case (a, Literal(Constant(x: String)), t) :: Nil if a.tpe == typeOf[v] =>
        q"${TermName(x)}: $t"
      case _ => error(s"malformed type-level literal: $dealiased")
    }
  }

  /** Build and and throw a [[TypecheckException]]. */
  def error(msg: String): Nothing
}
