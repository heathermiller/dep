package ch.epfl.lamp.dep
package internal

import collection.mutable.HashMap
import scala.reflect.api.Universe
import scala.reflect.macros.blackbox.Context
import scala.tools.reflect.ToolBox


/**
 * Bundle for evaluating trees to runtime values.
 */
trait TreeEvaluator {

  val c: Context
  
  import c.universe._

  // /** HashMap for caching evaluated trees. */
  // private lazy val valueCache = new HashMap[TreeWrapper, Any]

  // /**
  //  * Wrapper class to allow hash-consing trees based on structural
  //  * equality.
  //  */
  // private final class TreeWrapper(val u: Universe)(val t: u.Tree) {
  //   override def equals(that: Any): Boolean =
  //     that.isInstanceOf[TreeWrapper] &&
  //       t.equalsStructure(List(1))//that.asInstanceOf[TreeWrapper].t: Int)
  //   override def hashCode: Int = t.hashCode
  //   override def toString: String = t.toString
  // }

  def dealiasType(t: Type): Type = {
    val t2 = t.dealias
    appliedType(t2.typeConstructor, t2.typeArgs map {
      t => dealiasType(t)
    })
  }

  /** A tree transformer for de-aliasing types. */
  object TypeDealiaser extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case t: TypeTree => TypeTree(dealiasType(t.tpe))
      case _ => super.transform(tree)
    }
  }

  /** Evaluate `tree` to the runtime value it represents. */
  def eval[A](tree: Tree): A = {

    // Expand the tree and extract literals directly if possible.
    c.typecheck(tree) match {
      case Literal(Constant(x)) => x.asInstanceOf[A]  // Shortcut evaluation
      case t => {
        // De-alias all types in the tree to minimize the risk of
        // local (uncompiled) type aliases leaking through to the
        // toolbox compiler and causing errors.
        val dealiased = TypeDealiaser.transform(c.untypecheck(t))

        import scala.reflect.runtime.{ universe => ru }

        // Convert the compile-time tree to a run-time tree.
        val importer = ru.internal.createImporter(c.universe).asInstanceOf[
            ru.Importer { val from: c.universe.type }]
        val imported = importer.importTree(dealiased)

        // Attempt evaluation
        println(s"Attempting to evaluate $imported...")
        val r = TreeEvaluator.toolBox.eval(imported).asInstanceOf[A]
        println(s"$imported evaluated to $r.")
        r
      }
    }
  }
}

/**
 * Companion object holding the toolbox compiler instance.
 *
 * We keep around a toolbox compiler (and its reflection mirror) in
 * this object so we do not have to create one every time a tree needs
 * to be evaluated.
 */
object TreeEvaluator {

  import scala.reflect.runtime.universe._

  /** The reflection mirror used for evaluating trees. */
  private lazy val mirror = runtimeMirror(this.getClass.getClassLoader)

  /** The toolbox compiler used for evaluating trees. */
  private lazy val toolBox = mirror.mkToolBox()
}
