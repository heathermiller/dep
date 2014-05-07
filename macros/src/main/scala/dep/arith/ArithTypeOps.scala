package ch.epfl.lamp.dep
package arith

import scala.reflect.macros.whitebox.Context

import internal.TreeEvaluator


// Constants

/** Type-level zero literal. */
sealed trait Zero
object Zero {
  def apply: Any = macro applyImpl
  def applyImpl(c: Context) = {
    import c.universe._
    q"0"
  }
}


// Unary arithmetic operators

/** Type-level successor operator. */
sealed trait Succ[X]
object Succ {
  def apply(x: Any): Any = macro applyImpl
  def applyImpl(c: Context)(x: c.Tree) = {
    import c.universe._
    val default = q"$x + 1"
    x match {
      // Constant folding for primitive types
      case Literal(Constant(x)) => x match {
        case x: Byte   => Literal(Constant(x + 1))
        case x: Char   => Literal(Constant(x + 1))
        case x: Short  => Literal(Constant(x + 1))
        case x: Int    => Literal(Constant(x + 1))
        case x: Long   => Literal(Constant(x + 1))
        case x: Float  => Literal(Constant(x + 1))
        case x: Double => Literal(Constant(x + 1))
        case x: String => Literal(Constant(x + 1))
        case _ => default
      }
      case _ => default
    }
  }
}

// Binary arithmetic operators

/** Type-level addition. */
sealed trait +[X, Y]
object + {
  def apply(x: Any, y: Any): Any = macro applyImpl
  def applyImpl(c: Context)(x: c.Tree, y: c.Tree) = {
    import c.universe._
    import ArithHelpers._
    val default = q"$x + $y"
    (x, y) match {
      // Constant folding for primitive types
      case (Literal(Constant(x)), Literal(Constant(y))) => (x, y) match {
        case (x: String, _)  => Literal(Constant(x + y.toString))
        case (_, y: String)  => Literal(Constant(x.toString + y))
        case _ if isPrimitiveNumeric(x) && isPrimitiveNumeric(y) =>
          (x, y) match {
            case (x: Double, _) => Literal(Constant(x + toDouble(y)))
            case (_, y: Double) => Literal(Constant(toDouble(x) + y))
            case (x: Float, _)  => Literal(Constant(x + toFloat(y)))
            case (_, y: Float)  => Literal(Constant(toFloat(x) + y))
            case (x: Long, _)   => Literal(Constant(x + toLong(y)))
            case (_, y: Long)   => Literal(Constant(toLong(x) + y))
            case _              => Literal(Constant(toInt(x) + toInt(y)))
          }
        case _ => default
      }
      case _ => default
    }
  }
}


/** Type-level subtraction. */
sealed trait -[X, Y]
object - {
  def apply(x: Any, y: Any): Any = macro applyImpl
  def applyImpl(c: Context)(x: c.Tree, y: c.Tree) = {
    import c.universe._
    import ArithHelpers._
    (x, y) match {
      // Constant folding for primitive numeric types.
      case (Literal(Constant(x)), Literal(Constant(y)))
          if isPrimitiveNumeric(x) && isPrimitiveNumeric(y) =>
        (x, y) match {
          case (x: Double, _) => Literal(Constant(x - toDouble(y)))
          case (_, y: Double) => Literal(Constant(toDouble(x) - y))
          case (x: Float, _)  => Literal(Constant(x - toFloat(y)))
          case (_, y: Float)  => Literal(Constant(toFloat(x) - y))
          case (x: Long, _)   => Literal(Constant(x - toLong(y)))
          case (_, y: Long)   => Literal(Constant(toLong(x) - y))
          case _              => Literal(Constant(toInt(x) - toInt(y)))
        }
      case _ => q"$x - $y"
    }
  }
}

/** Type-level multiplication. */
sealed trait *[X, Y]
object * {
  def apply(x: Any, y: Any): Any = macro applyImpl
  def applyImpl(c: Context)(x: c.Tree, y: c.Tree) = {
    import c.universe._
    import ArithHelpers._
    val default = q"$x * $y"
    (x, y) match {
      // Constant folding for primitive types
      case (Literal(Constant(x)), Literal(Constant(y))) => (x, y) match {
        case (x: String, y: Int)  => Literal(Constant(x * y))
        case _ if isPrimitiveNumeric(x) && isPrimitiveNumeric(y) =>
          (x, y) match {
            case (x: Double, _) => Literal(Constant(x * toDouble(y)))
            case (_, y: Double) => Literal(Constant(toDouble(x) * y))
            case (x: Float, _)  => Literal(Constant(x * toFloat(y)))
            case (_, y: Float)  => Literal(Constant(toFloat(x) * y))
            case (x: Long, _)   => Literal(Constant(x * toLong(y)))
            case (_, y: Long)   => Literal(Constant(toLong(x) * y))
            case _              => Literal(Constant(toInt(x) * toInt(y)))
          }
        case _ => default
      }
      case _ => default
    }
  }
}

/** Type-level division. */
sealed trait /[X, Y]
object / {
  def apply(x: Any, y: Any): Any = macro applyImpl
  def applyImpl(c: Context)(x: c.Tree, y: c.Tree) = {
    import c.universe._
    import ArithHelpers._
    (x, y) match {
      // Constant folding for primitive numeric types.
      case (Literal(Constant(x)), Literal(Constant(y)))
          if isPrimitiveNumeric(x) && isPrimitiveNumeric(y) =>
        (x, y) match {
          case (x: Double, _) => Literal(Constant(x / toDouble(y)))
          case (_, y: Double) => Literal(Constant(toDouble(x) / y))
          case (x: Float, _)  => Literal(Constant(x / toFloat(y)))
          case (_, y: Float)  => Literal(Constant(toFloat(x) / y))
          case (x: Long, _)   => Literal(Constant(x / toLong(y)))
          case (_, y: Long)   => Literal(Constant(toLong(x) / y))
          case _              => Literal(Constant(toInt(x) / toInt(y)))
        }
      case _ => q"$x / $y"
    }
  }
}


// Comparisons

/** Type-level less-than comparison. */
sealed trait <[X, Y]
object < {
  def apply(x: Any, y: Any): Any = macro applyImpl
  def applyImpl(c: Context)(x: c.Tree, y: c.Tree) = {
    import c.universe._
    import ArithHelpers._
    (x, y) match {
      // Constant folding for primitive numeric types.
      case (Literal(Constant(x)), Literal(Constant(y)))
          if isPrimitiveNumeric(x) && isPrimitiveNumeric(y) =>
        (x, y) match {
          case (x: Double, _) => Literal(Constant(x < toDouble(y)))
          case (_, y: Double) => Literal(Constant(toDouble(x) < y))
          case (x: Float, _)  => Literal(Constant(x < toFloat(y)))
          case (_, y: Float)  => Literal(Constant(toFloat(x) < y))
          case (x: Long, _)   => Literal(Constant(x < toLong(y)))
          case (_, y: Long)   => Literal(Constant(toLong(x) < y))
          case _              => Literal(Constant(toInt(x) < toInt(y)))
        }
      case _ => q"$x < $y"
    }
  }
}

/** Type-level less-than-or-equal comparison. */
sealed trait <=[X, Y]
object <= {
  def apply(x: Any, y: Any): Any = macro applyImpl
  def applyImpl(c: Context)(x: c.Tree, y: c.Tree) = {
    import c.universe._
    import ArithHelpers._
    (x, y) match {
      // Constant folding for primitive numeric types.
      case (Literal(Constant(x)), Literal(Constant(y)))
          if isPrimitiveNumeric(x) && isPrimitiveNumeric(y) =>
        (x, y) match {
          case (x: Double, _) => Literal(Constant(x <= toDouble(y)))
          case (_, y: Double) => Literal(Constant(toDouble(x) <= y))
          case (x: Float, _)  => Literal(Constant(x <= toFloat(y)))
          case (_, y: Float)  => Literal(Constant(toFloat(x) <= y))
          case (x: Long, _)   => Literal(Constant(x <= toLong(y)))
          case (_, y: Long)   => Literal(Constant(toLong(x) <= y))
          case _              => Literal(Constant(toInt(x) <= toInt(y)))
        }
      case _ => q"$x <= $y"
    }
  }
}

/** Type-level greater-than comparison. */
sealed trait >[X, Y]
object > {
  def apply(x: Any, y: Any): Any = macro applyImpl
  def applyImpl(c: Context)(x: c.Tree, y: c.Tree) = {
    import c.universe._
    import ArithHelpers._
    (x, y) match {
      // Constant folding for primitive numeric types.
      case (Literal(Constant(x)), Literal(Constant(y)))
          if isPrimitiveNumeric(x) && isPrimitiveNumeric(y) =>
        (x, y) match {
          case (x: Double, _) => Literal(Constant(x > toDouble(y)))
          case (_, y: Double) => Literal(Constant(toDouble(x) > y))
          case (x: Float, _)  => Literal(Constant(x > toFloat(y)))
          case (_, y: Float)  => Literal(Constant(toFloat(x) > y))
          case (x: Long, _)   => Literal(Constant(x > toLong(y)))
          case (_, y: Long)   => Literal(Constant(toLong(x) > y))
          case _              => Literal(Constant(toInt(x) > toInt(y)))
        }
      case _ => q"$x > $y"
    }
  }
}

/** Type-level greater-than-or-equal comparison. */
sealed trait >=[X, Y]
object >= {
  def apply(x: Any, y: Any): Any = macro applyImpl
  def applyImpl(c: Context)(x: c.Tree, y: c.Tree) = {
    import c.universe._
    import ArithHelpers._
    (x, y) match {
      // Constant folding for primitive numeric types.
      case (Literal(Constant(x)), Literal(Constant(y)))
          if isPrimitiveNumeric(x) && isPrimitiveNumeric(y) =>
        (x, y) match {
          case (x: Double, _) => Literal(Constant(x >= toDouble(y)))
          case (_, y: Double) => Literal(Constant(toDouble(x) >= y))
          case (x: Float, _)  => Literal(Constant(x >= toFloat(y)))
          case (_, y: Float)  => Literal(Constant(toFloat(x) >= y))
          case (x: Long, _)   => Literal(Constant(x >= toLong(y)))
          case (_, y: Long)   => Literal(Constant(toLong(x) >= y))
          case _              => Literal(Constant(toInt(x) >= toInt(y)))
        }
      case _ => q"$x >= $y"
    }
  }
}


// Operator type classes

/** Base trait for generic unary operator (constant) type class. */
trait NullaryOperator[+C] { def apply: C }

/** Base trait for generic unary operator type class. */
trait UnaryOperator[-A, +C] { def apply(x: A): C }

/** Base trait for generic binary operator type class. */
trait BinaryOperator[-A, -B, +C] { def apply(x: A, y: B): C }

/** Generic type class for addition. */
trait Operator_+[-A, -B, +C] extends BinaryOperator[A, B, C]
object Operator_+ {

  // Default type class instances for primitive types.

  /** Integer addition. */
  implicit object Int_+ extends Operator_+[Int, Int, Int] {
    def apply(x: Int, y: Int): Int = x + y
  }

  /** Long integer addition. */
  implicit object Long_+ extends Operator_+[Long, Long, Long] {
    def apply(x: Long, y: Long): Long = x + y
  }

  /** Floating-point addition. */
  implicit object Float_+ extends Operator_+[Float, Float, Float] {
    def apply(x: Float, y: Float): Float = x + y
  }

  /** Double-precision floating-point addition. */
  implicit object Double_+ extends Operator_+[Double, Double, Double] {
    def apply(x: Double, y: Double): Double = x + y
  }

  /** String concatenation. */
  implicit object String_+ extends Operator_+[String, String, String] {
    def apply(x: String, y: String): String = x.toString + y.toString
  }
}

/** Generic type class for subtraction. */
trait Operator_-[-A, -B, +C] extends BinaryOperator[A, B, C]
object Operator_- {

  // Default type class instances for primitive types.

  /** Integer subtraction. */
  implicit object Int_- extends Operator_-[Int, Int, Int] {
    def apply(x: Int, y: Int): Int = x - y
  }

  /** Long integer subtraction. */
  implicit object Long_- extends Operator_-[Long, Long, Long] {
    def apply(x: Long, y: Long): Long = x - y
  }

  /** Floating-point subtraction. */
  implicit object Float_- extends Operator_-[Float, Float, Float] {
    def apply(x: Float, y: Float): Float = x - y
  }

  /** Double-precision floating-point subtraction. */
  implicit object Double_- extends Operator_-[Double, Double, Double] {
    def apply(x: Double, y: Double): Double = x - y
  }
}

/** Generic type class for multiplication. */
trait Operator_*[-A, -B, +C] extends BinaryOperator[A, B, C]
object Operator_* {

  // Default type class instances for primitive types.

  /** Integer multiplication. */
  implicit object Int_* extends Operator_*[Int, Int, Int] {
    def apply(x: Int, y: Int): Int = x * y
  }

  /** Long integer multiplication. */
  implicit object Long_* extends Operator_*[Long, Long, Long] {
    def apply(x: Long, y: Long): Long = x * y
  }

  /** Floating-point multiplication. */
  implicit object Float_* extends Operator_*[Float, Float, Float] {
    def apply(x: Float, y: Float): Float = x * y
  }

  /** Double-precision floating-point multiplication. */
  implicit object Double_* extends Operator_*[Double, Double, Double] {
    def apply(x: Double, y: Double): Double = x * y
  }

  /** String repetition. */
  implicit object StringInt_* extends Operator_*[String, Int, String] {
    def apply(x: String, y: Int): String = x * y
  }

  /** String repetition. */
  implicit object IntString_* extends Operator_*[Int, String, String] {
    def apply(x: Int, y: String): String = y * x
  }
}

/** Generic type class for division. */
trait Operator_/[-A, -B, +C] extends BinaryOperator[A, B, C]
object Operator_/ {

  // Default type class instances for primitive types.

  /** Integer division. */
  implicit object Int_/ extends Operator_/[Int, Int, Int] {
    def apply(x: Int, y: Int): Int = x / y
  }

  /** Long integer division. */
  implicit object Long_/ extends Operator_/[Long, Long, Long] {
    def apply(x: Long, y: Long): Long = x / y
  }

  /** Floating-point division. */
  implicit object Float_/ extends Operator_/[Float, Float, Float] {
    def apply(x: Float, y: Float): Float = x / y
  }

  /** Double-precision floating-point division. */
  implicit object Double_/ extends Operator_/[Double, Double, Double] {
    def apply(x: Double, y: Double): Double = x / y
  }
}

/** Generic type class for less-than comparison. */
trait Operator_<[-A, -B, +C] extends BinaryOperator[A, B, C]
object Operator_< {

  // Default type class instances for primitive types.

  /** Integer less-than comparison. */
  implicit object Int_< extends Operator_<[Int, Int, Boolean] {
    def apply(x: Int, y: Int): Boolean = x < y
  }

  /** Long integer less-than comparison. */
  implicit object Long_< extends Operator_<[Long, Long, Boolean] {
    def apply(x: Long, y: Long): Boolean = x < y
  }

  /** Floating-point less-than comparison. */
  implicit object Float_< extends Operator_<[Float, Float, Boolean] {
    def apply(x: Float, y: Float): Boolean = x < y
  }

  /** Double-precision floating-point less-than comparison. */
  implicit object Double_< extends Operator_<[Double, Double, Boolean] {
    def apply(x: Double, y: Double): Boolean = x < y
  }

  /** String less-than comparison. */
  implicit object String_< extends Operator_<[String, String, Boolean] {
    def apply(x: String, y: String): Boolean = x < y
  }
}

/** Generic type class for less-than-or-equal comparison. */
trait Operator_<=[-A, -B, +C] extends BinaryOperator[A, B, C]
object Operator_<= {

  // Default type class instances for primitive types.

  /** Integer less-than-or-equal comparison. */
  implicit object Int_<= extends Operator_<=[Int, Int, Boolean] {
    def apply(x: Int, y: Int): Boolean = x <= y
  }

  /** Long integer less-than-or-equal comparison. */
  implicit object Long_<= extends Operator_<=[Long, Long, Boolean] {
    def apply(x: Long, y: Long): Boolean = x <= y
  }

  /** Floating-point less-than-or-equal comparison. */
  implicit object Float_<= extends Operator_<=[Float, Float, Boolean] {
    def apply(x: Float, y: Float): Boolean = x <= y
  }

  /** Double-precision floating-point less-than-or-equal comparison. */
  implicit object Double_<= extends Operator_<=[Double, Double, Boolean] {
    def apply(x: Double, y: Double): Boolean = x <= y
  }

  /** String less-than-or-equal comparison. */
  implicit object String_<= extends Operator_<=[String, String, Boolean] {
    def apply(x: String, y: String): Boolean = x <= y
  }
}

/** Generic type class for less-than comparison. */
trait Operator_>[-A, -B, +C] extends BinaryOperator[A, B, C]
object Operator_> {

  // Default type class instances for primitive types.

  /** Integer greater-than. */
  implicit object Int_> extends Operator_>[Int, Int, Boolean] {
    def apply(x: Int, y: Int): Boolean = x > y
  }

  /** Long integer greater-than comparison. */
  implicit object Long_> extends Operator_>[Long, Long, Boolean] {
    def apply(x: Long, y: Long): Boolean = x > y
  }

  /** Floating-point greater-than comparison. */
  implicit object Float_> extends Operator_>[Float, Float, Boolean] {
    def apply(x: Float, y: Float): Boolean = x > y
  }

  /** Double-precision floating-point greater-than comparison. */
  implicit object Double_> extends Operator_>[Double, Double, Boolean] {
    def apply(x: Double, y: Double): Boolean = x > y
  }

  /** String greater-than comparison. */
  implicit object String_> extends Operator_>[String, String, Boolean] {
    def apply(x: String, y: String): Boolean = x > y
  }
}

/** Generic type class for less-than-or-equal comparison. */
trait Operator_>=[-A, -B, +C] extends BinaryOperator[A, B, C]
object Operator_>= {

  // Default type class instances for primitive types.

  /** Integer greater-than-or-equal comparison. */
  implicit object Int_>= extends Operator_>=[Int, Int, Boolean] {
    def apply(x: Int, y: Int): Boolean = x >= y
  }

  /** Long integer greater-than-or-equal comparison. */
  implicit object Long_>= extends Operator_>=[Long, Long, Boolean] {
    def apply(x: Long, y: Long): Boolean = x >= y
  }

  /** Floating-point greater-than-or-equal comparison. */
  implicit object Float_>= extends Operator_>=[Float, Float, Boolean] {
    def apply(x: Float, y: Float): Boolean = x >= y
  }

  /** Double-precision floating-point greater-than-or-equal comparison. */
  implicit object Double_>= extends Operator_>=[Double, Double, Boolean] {
    def apply(x: Double, y: Double): Boolean = x >= y
  }

  /** String greater-than-or-equal comparison. */
  implicit object String_>= extends Operator_>=[String, String, Boolean] {
    def apply(x: String, y: String): Boolean = x >= y
  }
}


// Helpers

/** A collection of helper methods for type-level arithmetic. */
object ArithHelpers {
  def isPrimitiveNumeric(x: Any): Boolean = x match {
    case _: Byte | _: Char | _: Short | _: Int | _: Long
       | _: Float | _: Double => true
    case _ => false
  }

  def toInt(x: Any): Int = x match {
    case x: Byte  => x.toInt
    case x: Char  => x.toInt
    case x: Short => x.toInt
    case x: Int   => x
    case _ => throw new java.lang.ClassCastException
  }

  def toLong(x: Any): Long = x match {
    case x: Byte  => x.toLong
    case x: Char  => x.toLong
    case x: Short => x.toLong
    case x: Int   => x.toLong
    case x: Long  => x
    case _ => throw new java.lang.ClassCastException
  }

  def toFloat(x: Any): Float = x match {
    case x: Byte  => x.toFloat
    case x: Char  => x.toFloat
    case x: Short => x.toFloat
    case x: Int   => x.toFloat
    case x: Long  => x.toFloat
    case x: Float => x
    case _ => throw new java.lang.ClassCastException
  }

  def toDouble(x: Any): Double = x match {
    case x: Byte   => x.toDouble
    case x: Char   => x.toDouble
    case x: Short  => x.toDouble
    case x: Int    => x.toDouble
    case x: Long   => x.toDouble
    case x: Float  => x.toDouble
    case x: Double => x
    case _ => throw new java.lang.ClassCastException
  }
}
