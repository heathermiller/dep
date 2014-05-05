package ch.epfl.lamp.dep
package internal

import scala.reflect.macros.whitebox.Context


/**
 * Methods for evaluating trees to runtime values.
 */
object TreeEvaluator {

  def dealiasType(c: Context)(t: c.Type): c.Type = {
    import c.universe._
    val t2 = t.dealias
    appliedType(t2.typeConstructor, t2.typeArgs map {
      t => dealiasType(c)(t)
    })
  }

  /** Evaluate `tree` to the runtime value it represents. */
  def eval[A](c: Context)(tree: c.Tree): A = {

    import c.universe._

    // A tree transformer for de-aliasing types.
    object TypeDealiaser extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case t: TypeTree => TypeTree(dealiasType(c)(t.tpe))
        case _ => super.transform(tree)
      }
    }

    // Expand the tree and extract literals directly if possible.
    c.typecheck(tree) match {
      case Literal(Constant(x)) => x.asInstanceOf[A]
      case t => {
        // De-alias all types in the tree and evaluate it using
        // reflective compilation.
        val t2 = TypeDealiaser.transform(t)
        println(s"Attempting to evaluate $t2...")
        c.eval(c.Expr[A](c.untypecheck(t2)))
      }
    }
  }

  /**
   * Evaluate the interpreter `ip` to the runtime value it represents.
   */
  def evalInterpreter[A](c: Context)(ip: c.Tree): A = {
    import c.universe._
    eval[A](c)(q"$ip.value")
  }

  /**
   * Helper method for evaluating unary operations at compile time.
   *
   * Given an unary operation `op(x)` returning a value of literal
   * type and a tree for the arguments `x` of the operation, this
   * method will return the literal (tree) `op(x)`.
   */
  def evalAndApplyOp[A, B](c: Context)(x: c.Tree, op: A => B): c.Tree = {
    import c.universe._
    Literal(Constant(TreeEvaluator.eval[A](c)(x)))
  }

  /**
   * Helper method for evaluating binary operations at compile time.
   *
   * Given a binary operation `op(x, y)` returning a value of literal
   * type and trees for the arguments `x` and `y` of the operation,
   * this method will return the literal (tree) `op(x, y)`.
   */
  def evalAndApplyBinOp[A, B, C](c: Context)(
    x: c.Tree, y: c.Tree, op: (A, B) => C): c.Tree = {
    import c.universe._
    val xv = TreeEvaluator.eval[A](c)(x)
    val yv = TreeEvaluator.eval[B](c)(y)
    Literal(Constant(op(xv, yv)))
  }
}
