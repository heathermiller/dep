package ch.epfl.lamp.dep
package internal

import scala.reflect.macros.whitebox.Context

/**
 */
trait ExprEvaluator {

  /** The context within which expressions should be evaluated. */
  val c: Context

  import c.universe._

  /** A tree transformer for de-aliasing types. */
  object TypeDealiaser extends Transformer {
    def dealias(t: Type): Type = {
      val t2 = t.dealias
      appliedType(t2.typeConstructor, t2.typeArgs.map(dealias))
    }
    override def transform(tree: Tree): Tree = tree match {
      case t: TypeTree => TypeTree(dealias(t.tpe))
      case _ => super.transform(tree)
    }
  }

  def eval[A](expr: c.Expr[A]): A = {
    // Extract the tree and check whether it is a literal.
    expr.tree match {
      case Literal(Constant(x)) => x.asInstanceOf[A]
      case t => {
        // De-alias all types in the tree and evaluate it using
        // reflective compilation.
        val t2 = TypeDealiaser.transform(t)
        c.eval(c.Expr[A](c.untypecheck(t2)))
      }
    }
  }
}
