package ch.epfl.lamp.dep
package internal

import scala.reflect.macros.whitebox.Context

/**
 * Support for macro value classes.
 *
 * Macro value classes are like value classes, but more flexible.
 */
object MacroValueClass {

  /** Generic value field accessor macro implementation. */
  def unwrap(c: Context): c.Tree = {

    import c.universe._

    // Determine the prefix and field-name of the macro application.
    val (prefix, name) = c.macroApplication match {
      case Select(prefix, TermName(name)) => (prefix, name)
      case _ => c.abort(c.enclosingPosition,
        "cannot expand non-field value access: " + c.macroApplication)
    }
    val fname = TermName("internal$" + name)  // Internal field name.

    // Check whether the prefix is an object instantiation.
    val ctorName = termNames.CONSTRUCTOR
    prefix match {

      // Object instantiation.  Find the parameter corresponding to
      // the internal field name and pick the constructor argument at
      // the corresponding position.
      case Apply(Select(New(_), n), args) if n == ctorName => {
        val ctorSym = prefix.tpe.member(ctorName)
        val ctorMtd = ctorSym.asTerm.alternatives.head.asMethod
        val ctorParamIdx =
          ctorMtd.paramLists.head.indexWhere(p => p.name == fname)
        if (ctorParamIdx < 0) c.abort(c.enclosingPosition,
          "primary constructor of type " + prefix.tpe +
            " has no parameter named " + name)
        args(ctorParamIdx)
      }

      // Not an object instantiation.  Fall back to field access.
      case _ => {
        q"$prefix.$fname"
      }
    }
  }

  /** Helper function for emitting object instantiations. */
  def wrap(c: Context)(tpe: c.Type, args: c.Tree*): c.Tree = {
    import c.universe._
    q"new $tpe(..${args.toList})"
  }

  /**
   * Generic macro implementation for single-argument factory methods.
   */
  def wrap1[T: c.WeakTypeTag](c: Context)(value: c.Tree): c.Tree =
    wrap(c)(implicitly[c.WeakTypeTag[T]].tpe, value)
}
