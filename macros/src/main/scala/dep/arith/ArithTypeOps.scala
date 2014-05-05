package ch.epfl.lamp.dep
package arith

import scala.reflect.macros.whitebox.Context

import internal.TreeEvaluator


// FIXME: Remove all this boiler plate.  Generate these traits and
// their interpreters using a macro annotation.

// Peano literals

/** Type-level zero literal. */
sealed trait Zero extends Pt[Int]
object Zero {
  implicit def eval: Interpreter[Int, Zero] = macro evalImpl
  def evalImpl(c: Context) = Interpreter.fromLiteral[Int, Zero](c)(0)
}

/** Type-level successor operator. */
sealed trait Succ[X <: Pt[Int]] extends Pt[Int]
object Succ {
  implicit def eval[X <: Pt[Int]](
    implicit xip: Interpreter[Int, X]): Interpreter[Int, Succ[X]] =
    macro evalImpl[X]
  def evalImpl[X <: Pt[Int]: c.WeakTypeTag](c: Context)(xip: c.Tree) =
    Interpreter.fromLiteralOp[Int, Int, Succ[X]](c)(xip, _ + 1)
}


// Arithmetic operators

/** Type-level addition. */
sealed trait +[X, Y]
object + {
  implicit def eval[A, B, C, X, Y](
    implicit xip: Interpreter[A, X], yip: Interpreter[B, Y],
    op: Operator_+[A, B, C]): Interpreter[C, X + Y] =
    macro BinOpInterpreterImpl.evalImpl[C, X, Y, +]
}

/** Generic macro type class for addition. */
trait Operator_+[A, B, C] extends BinOperator[A, B, C]
object Operator_+ {

  // Optimized type class instances for "literal" types.

  /** Compile-time integer addition. */
  implicit object Int_+ extends Operator_+[Int, Int, Int] {
    def internal$apply(x: Int, y: Int) = x + y
    override def apply(x: Int, y: Int): Int = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Int, Int, Int](c)(x, y, _+_)
  }

  /** Compile-time long integer addition. */
  implicit object Long_+ extends Operator_+[Long, Long, Long] {
    def internal$apply(x: Long, y: Long) = x + y
    override def apply(x: Long, y: Long): Long = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Long, Long, Long](c)(x, y, _+_)
  }

  /** Compile-time floating-point addition. */
  implicit object Float_+ extends Operator_+[Float, Float, Float] {
    def internal$apply(x: Float, y: Float) = x + y
    override def apply(x: Float, y: Float): Float = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Float, Float, Float](c)(x, y, _+_)
  }

  /** Compile-time double-precision floating-point addition. */
  implicit object Double_+ extends Operator_+[Double, Double, Double] {
    def internal$apply(x: Double, y: Double) = x + y
    override def apply(x: Double, y: Double): Double = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Double, Double, Double](c)(x, y, _+_)
  }
}


/** Type-level subtraction. */
sealed trait -[X, Y]
object - {
  implicit def eval[A, B, C, X, Y](
    implicit xip: Interpreter[A, X], yip: Interpreter[B, Y],
    op: Operator_-[A, B, C]): Interpreter[C, X - Y] =
    macro BinOpInterpreterImpl.evalImpl[C, X, Y, -]
}

/** Generic macro type class for subtraction. */
trait Operator_-[A, B, C] extends BinOperator[A, B, C]
object Operator_- {

  // Optimized type class instances for "literal" types.

  /** Compile-time integer subtraction. */
  implicit object Int_- extends Operator_-[Int, Int, Int] {
    def internal$apply(x: Int, y: Int) = x - y
    override def apply(x: Int, y: Int): Int = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Int, Int, Int](c)(x, y, _-_)
  }

  /** Compile-time long integer subtraction. */
  implicit object Long_- extends Operator_-[Long, Long, Long] {
    def internal$apply(x: Long, y: Long) = x - y
    override def apply(x: Long, y: Long): Long = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Long, Long, Long](c)(x, y, _-_)
  }

  /** Compile-time floating-point subtraction. */
  implicit object Float_- extends Operator_-[Float, Float, Float] {
    def internal$apply(x: Float, y: Float) = x - y
    override def apply(x: Float, y: Float): Float = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Float, Float, Float](c)(x, y, _-_)
  }

  /** Compile-time double-precision floating-point subtraction. */
  implicit object Double_- extends Operator_-[Double, Double, Double] {
    def internal$apply(x: Double, y: Double) = x - y
    override def apply(x: Double, y: Double): Double = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Double, Double, Double](c)(x, y, _-_)
  }
}


/** Type-level integer multiplication. */
sealed trait *[X, Y]
object * {
  implicit def eval[A, B, C, X, Y](
    implicit xip: Interpreter[A, X], yip: Interpreter[B, Y],
    op: Operator_*[A, B, C]): Interpreter[C, *[X, Y]] =
    macro BinOpInterpreterImpl.evalImpl[C, X, Y, *]
}

/** Generic macro type class for multiplication. */
trait Operator_*[A, B, C] extends BinOperator[A, B, C]
object Operator_* {

  // Optimized type class instances for "literal" types.

  /** Compile-time integer multiplication. */
  implicit object Int_* extends Operator_*[Int, Int, Int] {
    def internal$apply(x: Int, y: Int) = x * y
    override def apply(x: Int, y: Int): Int = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Int, Int, Int](c)(x, y, _*_)
  }

  /** Compile-time long integer multiplication. */
  implicit object Long_* extends Operator_*[Long, Long, Long] {
    def internal$apply(x: Long, y: Long) = x * y
    override def apply(x: Long, y: Long): Long = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Long, Long, Long](c)(x, y, _*_)
  }

  /** Compile-time floating-point multiplication. */
  implicit object Float_* extends Operator_*[Float, Float, Float] {
    def internal$apply(x: Float, y: Float) = x * y
    override def apply(x: Float, y: Float): Float = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Float, Float, Float](c)(x, y, _*_)
  }

  /** Compile-time double-precision floating-point multiplication. */
  implicit object Double_* extends Operator_*[Double, Double, Double] {
    def internal$apply(x: Double, y: Double) = x * y
    override def apply(x: Double, y: Double): Double = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Double, Double, Double](c)(x, y, _*_)
  }
}


/** Type-level division. */
sealed trait /[X, Y]
object / {
  implicit def eval[A, B, C, X, Y](
    implicit xip: Interpreter[A, X], yip: Interpreter[B, Y],
    op: Operator_/[A, B, C]): Interpreter[C, X / Y] =
    macro BinOpInterpreterImpl.evalImpl[C, X, Y, /]
}

/** Generic macro type class for division. */
trait Operator_/[A, B, C] extends BinOperator[A, B, C]
object Operator_/ {

  // Optimized type class instances for "literal" types.

  /** Compile-time integer division. */
  implicit object Int_/ extends Operator_/[Int, Int, Int] {
    def internal$apply(x: Int, y: Int) = x / y
    override def apply(x: Int, y: Int): Int = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Int, Int, Int](c)(x, y, _/_)
  }

  /** Compile-time long integer division. */
  implicit object Long_/ extends Operator_/[Long, Long, Long] {
    def internal$apply(x: Long, y: Long) = x / y
    override def apply(x: Long, y: Long): Long = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Long, Long, Long](c)(x, y, _/_)
  }

  /** Compile-time floating-point division. */
  implicit object Float_/ extends Operator_/[Float, Float, Float] {
    def internal$apply(x: Float, y: Float) = x / y
    override def apply(x: Float, y: Float): Float = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Float, Float, Float](c)(x, y, _/_)
  }

  /** Compile-time double-precision floating-point division. */
  implicit object Double_/ extends Operator_/[Double, Double, Double] {
    def internal$apply(x: Double, y: Double) = x / y
    override def apply(x: Double, y: Double): Double = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Double, Double, Double](c)(x, y, _/_)
  }
}


// Comparisons

/** Type-level less-than operator. */
trait <[X, Y] <: Pt[Boolean]
object < {
  implicit def eval[A, B, X, Y](
    implicit xip: Interpreter[A, X], yip: Interpreter[B, Y],
    op: Operator_<[A, B]): Interpreter[Boolean, X < Y] =
    macro BinOpInterpreterImpl.evalImpl[Boolean, X, Y, <]
}

/** Generic macro type class for less-than comparison. */
trait Operator_<[A, B] extends BinOperator[A, B, Boolean]
object Operator_< {

  // Optimized type class instances for "literal" types.

  /** Compile-time integer less-than comparison. */
  implicit object Int_< extends Operator_<[Int, Int] {
    def internal$apply(x: Int, y: Int) = x < y
    override def apply(x: Int, y: Int): Boolean = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Int, Int, Boolean](c)(x, y, _<_)
  }

  /** Compile-time long integer less-than comparison. */
  implicit object Long_< extends Operator_<[Long, Long] {
    def internal$apply(x: Long, y: Long) = x < y
    override def apply(x: Long, y: Long): Boolean = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Long, Long, Boolean](c)(x, y, _<_)
  }

  /** Compile-time floating-point less-than comparison. */
  implicit object Float_< extends Operator_<[Float, Float] {
    def internal$apply(x: Float, y: Float) = x < y
    override def apply(x: Float, y: Float): Boolean = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Float, Float, Boolean](c)(x, y, _<_)
  }

  /** Compile-time double-precision floating-point less-than comparison. */
  implicit object Double_< extends Operator_<[Double, Double] {
    def internal$apply(x: Double, y: Double) = x < y
    override def apply(x: Double, y: Double): Boolean = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Double, Double, Boolean](c)(x, y, _<_)
  }
}


/** Type-level less-than-or-equal operator. */
trait <=[X, Y] <: Pt[Boolean]
object <= {
  implicit def eval[A, B, X, Y](
    implicit xip: Interpreter[A, X], yip: Interpreter[B, Y],
    op: Operator_<=[A, B]): Interpreter[Boolean, X <= Y] =
    macro BinOpInterpreterImpl.evalImpl[Boolean, X, Y, <=]
}

/** Generic macro type class for less-than-or-equal comparison. */
trait Operator_<=[A, B] extends BinOperator[A, B, Boolean]
object Operator_<= {

  // Optimized type class instances for "literal" types.

  /** Compile-time integer less-than-or-equal comparison. */
  implicit object Int_<= extends Operator_<=[Int, Int] {
    def internal$apply(x: Int, y: Int) = x <= y
    override def apply(x: Int, y: Int): Boolean = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Int, Int, Boolean](c)(x, y, _<=_)
  }

  /** Compile-time long integer less-than-or-equal comparison. */
  implicit object Long_<= extends Operator_<=[Long, Long] {
    def internal$apply(x: Long, y: Long) = x <= y
    override def apply(x: Long, y: Long): Boolean = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Long, Long, Boolean](c)(x, y, _<=_)
  }

  /** Compile-time floating-point less-than-or-equal comparison. */
  implicit object Float_<= extends Operator_<=[Float, Float] {
    def internal$apply(x: Float, y: Float) = x <= y
    override def apply(x: Float, y: Float): Boolean = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Float, Float, Boolean](c)(x, y, _<=_)
  }

  /**
   * Compile-time double-precision floating-point less-than-or-equal
   * comparison.
   */
  implicit object Double_<= extends Operator_<=[Double, Double] {
    def internal$apply(x: Double, y: Double) = x <= y
    override def apply(x: Double, y: Double): Boolean = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Double, Double, Boolean](c)(x, y, _<=_)
  }
}


/** Type-level greater-than operator. */
trait >[X, Y] <: Pt[Boolean]
object > {
  implicit def eval[A, B, X, Y](
    implicit xip: Interpreter[A, X], yip: Interpreter[B, Y],
    op: Operator_>[A, B]): Interpreter[Boolean, X > Y] =
    macro BinOpInterpreterImpl.evalImpl[Boolean, X, Y, >]
}

/** Generic macro type class for greater-than comparison. */
trait Operator_>[A, B] extends BinOperator[A, B, Boolean]
object Operator_> {

  // Optimized type class instances for "literal" types.

  /** Compile-time integer greater-than comparison. */
  implicit object Int_> extends Operator_>[Int, Int] {
    def internal$apply(x: Int, y: Int) = x > y
    override def apply(x: Int, y: Int): Boolean = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Int, Int, Boolean](c)(x, y, _>_)
  }

  /** Compile-time long integer greater-than comparison. */
  implicit object Long_> extends Operator_>[Long, Long] {
    def internal$apply(x: Long, y: Long) = x > y
    override def apply(x: Long, y: Long): Boolean = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Long, Long, Boolean](c)(x, y, _>_)
  }

  /** Compile-time floating-point greater-than comparison. */
  implicit object Float_> extends Operator_>[Float, Float] {
    def internal$apply(x: Float, y: Float) = x > y
    override def apply(x: Float, y: Float): Boolean = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Float, Float, Boolean](c)(x, y, _>_)
  }

  /** Compile-time double-precision floating-point greater-than comparison. */
  implicit object Double_> extends Operator_>[Double, Double] {
    def internal$apply(x: Double, y: Double) = x > y
    override def apply(x: Double, y: Double): Boolean = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Double, Double, Boolean](c)(x, y, _>_)
  }
}


/** Type-level greater-than-or-equal operator. */
trait >=[X, Y] <: Pt[Boolean]
object >= {
  implicit def eval[A, B, X, Y](
    implicit xip: Interpreter[A, X], yip: Interpreter[B, Y],
    op: Operator_>=[A, B]): Interpreter[Boolean, X >= Y] =
    macro BinOpInterpreterImpl.evalImpl[Boolean, X, Y, >=]
}

/** Generic macro type class for greater-than-or-equal comparison. */
trait Operator_>=[A, B] extends BinOperator[A, B, Boolean]
object Operator_>= {

  // Optimized type class instances for "literal" types.

  /** Compile-time integer greater-than-or-equal comparison. */
  implicit object Int_>= extends Operator_>=[Int, Int] {
    def internal$apply(x: Int, y: Int) = x >= y
    override def apply(x: Int, y: Int): Boolean = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Int, Int, Boolean](c)(x, y, _>=_)
  }

  /** Compile-time long integer greater-than-or-equal comparison. */
  implicit object Long_>= extends Operator_>=[Long, Long] {
    def internal$apply(x: Long, y: Long) = x >= y
    override def apply(x: Long, y: Long): Boolean = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Long, Long, Boolean](c)(x, y, _>=_)
  }

  /** Compile-time floating-point greater-than-or-equal comparison. */
  implicit object Float_>= extends Operator_>=[Float, Float] {
    def internal$apply(x: Float, y: Float) = x >= y
    override def apply(x: Float, y: Float): Boolean = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Float, Float, Boolean](c)(x, y, _>=_)
  }

  /**
   * Compile-time double-precision floating-point
   * greater-than-or-equal comparison.
   */
  implicit object Double_>= extends Operator_>=[Double, Double] {
    def internal$apply(x: Double, y: Double) = x >= y
    override def apply(x: Double, y: Double): Boolean = macro impl
    def impl(c: Context)(x: c.Tree, y: c.Tree): c.Tree =
      TreeEvaluator.evalAndApplyBinOp[Double, Double, Boolean](c)(x, y, _>=_)
  }
}


// Helpers

/** Generic operator macro type class. */
trait BinOperator[A, B, C] {
  def apply(x: A, y: B): C = macro BinOperator.impl[A, B, C]
  def internal$apply(x: A, y: B): C
}
object BinOperator {
  def impl[A, B, C](c: Context)(x: c.Tree, y: c.Tree): c.Tree = {
    import c.universe._
    q"${c.prefix}.internal$$apply($x, $y)"
  }
}


/** Generic type-level binary operator interpreter macro bundle. */
object BinOpInterpreterImpl {

  /**
   * Generic interpreter macro implementation for type-level binary
   * operators.
   */
  def evalImpl[C, X, Y, O[_, _]](c: Context)(
    xip: c.Tree, yip: c.Tree, op: c.Tree)(
    implicit ctt: c.WeakTypeTag[C], xtt: c.WeakTypeTag[X],
    ytt: c.WeakTypeTag[Y], ott: c.WeakTypeTag[O[Any, Any]]) = {
    import c.universe._
    val ctor = ott.tpe.typeConstructor
    val rtp = appliedType(ctor, List(xtt.tpe, ytt.tpe))
    Interpreter.fromTreeAndTypes(c)(
      q"$op.apply($xip.value, $yip.value)", ctt.tpe, rtp)
  }
}
