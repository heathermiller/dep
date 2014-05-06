package ch.epfl.lamp.dep
package arith

import scala.reflect.ClassTag

import org.scalatest.{ FlatSpecLike, Matchers }

import internal.Proof


class VectorsAndMatrices {

  // Alias for `1` type-level literal
  type One = Pt[Int@lit(1)]

  // Matrices

  /** Generic fixed-size matrices. */
  class Mat[+T, M, N] protected (
    private[this] val data: Array[T], val width: Int)(
    implicit dt: ClassTag[T])
      extends Equals {

    @inline protected def apply(i: Int): T = data(i)
    @inline protected def apply(i: Int, j: Int): T = data(width * i + j)

    @inline def length: Int = data.length
    @inline def height: Int = length / width

    /** Element access. */
    def apply[I, J](
      implicit iip: Interpreter[Int, I], jip: Interpreter[Int, J],
      ipf: Proof[(Zero <= I) && (I < M)],
      jpf: Proof[(Zero <= J) && (J < N)]): T =
      this(interpret[Int, I], interpret[Int, J])

    /** Coordinate projection. */
    def apply[I](
      implicit iip: Interpreter[Int, I],
      ipf: Proof[(Zero <= I) && (I < *[M, N])]): T =
      data(interpret[Int, I])

    /** Applies `f` element-wise. */
    def foreach[R](f: T => R): Unit = data.foreach(f)

    /** Returns the result of applying `f` element-wise. */
    def map[R](f: T => R)(implicit rt: ClassTag[R]): Mat[R, M, N] =
      new Mat[R, M, N](Array.tabulate[R](length) { i => f(data(i)) }, width)

    /** Transpose operation. */
    def t: Mat[T, N, M] = {
      val resData = new Array[T](length)
      var q = 0
      (0 until width) foreach { j =>
        (0 until height) foreach { i =>
          resData(q) = this(i, j)
          q += 1
        }
      }
      new Mat[T, N, M](resData, height)
    }

    /** Matrix addition. */
    def +[S, R, I, J](that: Mat[S, I, J])(
      implicit add: Operator_+[T, S, R], rt: ClassTag[R],
      eq1: M =*= I, eq2: N =*= J): Mat[R, M, N] = new Mat[R, M, N](
        Array.tabulate[R](length) { k => add(this(k), that(k)) }, width)

    /** Scalar multiplication. */
    def *[S, R](x: S)(
      implicit mul: Operator_*[T, S, R], rt: ClassTag[R]): Mat[R, M, N] = {
      map { y => mul(y, x) }
    }

    /** Matrix multiplication. */
    def *[S, R, I, J](that: Mat[S, I, J])(
      implicit mul: Operator_*[T, S, R], add: Operator_+[R, R, R],
      rt: ClassTag[R], eq: N =*= I, pf: Proof[N > Zero])
        : Mat[R, M, J] = {
      val resHeight = this.height
      val resWidth = that.width
      val resLength =  resHeight * resWidth
      val resData = new Array[R](resLength)
      var q = 0
      (0 until resHeight) foreach { h =>
        (0 until resWidth) foreach { w =>
          resData(q) = mul(this(h, 0), that(0, w))
          (1 until this.width) foreach { k =>
            resData(q) = add(resData(q), mul(this(h, k), that(k, w)))
          }
          q += 1
        }
      }
      new Mat[R, M, J](resData, resWidth)
    }

    /** Conversion to nested vectors. */
    def asRowMajVec: Mat[Mat[T, N, One], M, One] = {
      new Mat[Mat[T, N, One], M, One](Array.tabulate[Mat[T, N, One]](height) {
        i => new Mat[T, N, One](Array.tabulate[T](width) {
          j => this(i, j)
        }, 1)
      }, 1)
    }

    /** Implicit conversion from 1x1 matrices to scalars. */
    implicit def asScalar(
      implicit eq1: M =*= One, eq2: M =*= N): T = data(0);

    /** Checks this matrix and `that` for equality. */
    override def equals(that: Any): Boolean = {
      if (that.isInstanceOf[Mat[_, _, _]]) {
        val that2 = that.asInstanceOf[Mat[_, _, _]]
        var res = (that2.canEqual(this)) &&
          (this.length == that2.length) &&
          (this.width == that2.width)
        var i = 0
        while ((res != false) && (i < length)) {
          res = res && (this(i) == that2(i))
          i += 1
        }
        res
      } else false
    }

    /** Checks this vector and `that` for equality. */
    def canEqual(that: Any): Boolean = that.isInstanceOf[Mat[_, _, _]]

    /** The hashcode of the underlying coordinate array of this vector. */
    override def hashCode: Int = width + data.hashCode

    /** Converts the vector to a string. */
    override def toString: String = data.mkString("[", ", ", "]")
  }

  object Mat {

    // Factory methods

    /** Factory method. */
    def tabulate[T, M, N](f: (Int, Int) => T)(
      implicit tt: ClassTag[T], mip: Interpreter[Int, M],
      nip: Interpreter[Int, N], mpf: Proof[N >= Zero],
      npf: Proof[M >= Zero]): Mat[T, M, N] = {
      val m = interpret[Int, M]
      val n = interpret[Int, N]
      val resData = new Array[T](m * n)
      var q = 0
      (0 until m) foreach { i =>
        (0 until n) foreach { j => resData(q) = f(i, j); q += 1 }
      }
      new Mat[T, M, N](resData, n)
    }

    /** Factory method. */
    def tabulate[T, M, N](f: Int => T)(
      implicit tt: ClassTag[T], mip: Interpreter[Int, M],
      nip: Interpreter[Int, N], mpf: Proof[N >= Zero],
      npf: Proof[M >= Zero]): Mat[T, M, N] = {
      val m = interpret[Int, M]
      val n = interpret[Int, N]
      new Mat[T, M, N](Array.tabulate[T](m * n) { i => f(i) }, n)
    }

    /** Factory method. */
    def fromRowMajVec[T, M, N, O1, O2](v: Mat[Mat[T, N, O1], M, O2])(
      implicit tt: ClassTag[T],
      mip: Interpreter[Int, M], nip: Interpreter[Int, N],
      mpf: Proof[N >= Zero], npf: Proof[M >= Zero],
      o1eq: O1 =*= One, o2eq: O2 =*= O1): Mat[T, M, N] = {
      Mat.tabulate[T, M, N] { (i: Int, j: Int) => v(i)(j) }
    }


    // Operator type classes

    /** Matrix addition type class. */
    final class Mat_+[-S, -T, +R, M, N, I, J](
      implicit add: Operator_+[S, T, R], rt: ClassTag[R],
      eq1: M =*= I, eq2: N =*= J)
        extends Operator_+[Mat[S, M, N], Mat[T, I, J], Mat[R, M, N]] {
      def internal$apply(a: Mat[S, M, N], b: Mat[T, I, J]) = a + b;
    }
    implicit def mat_+[S, T, R, M, N, I, J](
      implicit add: Operator_+[S, T, R], rt: ClassTag[R],
      eq1: M =*= I, eq2: N =*= J) =
        new Mat_+[S, T, R, M, N, I, J]

    /** Scalar multiplication type class. */
    final class MatScalar_*[-S, -T, +R, M, N](
      implicit mul: Operator_*[S, T, R], rt: ClassTag[R])
        extends Operator_*[Mat[S, M, N], T, Mat[R, M, N]] {
      def internal$apply(a: Mat[S, M, N], x: T) = a * x;
    }
    implicit def matScalar_*[S, T, R, M, N](
      implicit mul: Operator_*[S, T, R], rt: ClassTag[R]) =
        new MatScalar_*[S, T, R, M, N]

    /** Matrix multiplication type class. */
    final class MatMat_*[-S, -T, +R, M, N, I, J](
      implicit mul: Operator_*[S, T, R], add: Operator_+[R, R, R],
      rt: ClassTag[R], eq: N =*= I, pf: Proof[N > Zero])
        extends Operator_*[Mat[S, M, N], Mat[T, I, J], Mat[R, M, J]] {
      def internal$apply(a: Mat[S, M, N], b: Mat[T, I, J]) = a * b
    }
    implicit def matMat_*[S, T, R, M, N, I, J](
      implicit mul: Operator_*[S, T, R], add: Operator_+[R, R, R],
      rt: ClassTag[R], eq: N =*= I, pf: Proof[N > Zero]) =
      new MatMat_*[S, T, R, M, N, I, J]
  }

  object Vec {

    /** Factory method. */
    def tabulate[T, N](f: Int => T)(
      implicit tt: ClassTag[T], nip: Interpreter[Int, N],
      pf: Proof[N >= Zero]): Mat[T, N, One] =
      Mat.tabulate[T, N, One](f)
  }
}

class VecMatSpec extends VectorsAndMatrices with FlatSpecLike with Matchers {

  // Shortcuts
  type Two   = Pt[Int@lit(2)]
  type Three = Pt[Int@lit(3)]

  it should "do some linear algebra" in {

    // 3D coordinate base
    val delta: (Int, Int) => Double = { (x, y) => if (x == y) 1 else 0 }
    val e1 = Vec.tabulate[Double, Three](delta(0, _))
    val e2 = Vec.tabulate[Double, Three](delta(1, _))
    val e3 = Vec.tabulate[Double, Three](delta(2, _))

    e1.length should equal(3)
    e1[Zero] should equal(1)
    e1[One] should equal(0)
    e1[Two] should equal(0)

    e2.length should equal(3)
    e2[Zero] should equal(0)
    e2[One] should equal(1)
    e2[Two] should equal(0)

    e3.length should equal(3)
    e3[Zero] should equal(0)
    e3[One] should equal(0)
    e3[Two] should equal(1)

    // Addition and scalar multiplication
    val v = e1 + e2 * 2.0
    v.length should equal(3)
    v[Zero] should equal(1)
    v[One] should equal(2)
    v[Two] should equal(0)

    // Dot products
    (e1.t * e2).asScalar should equal (0)
    (v.t  * e2).asScalar should equal (v[One])

    // Outer products
    val a = Mat.tabulate[Double, Three, Three](List[Double](
      0, 0, 0,
      1, 2, 0,
      0, 0, 0
    ))
    (e2 * v.t) should equal (a)

    // Basic linear algebra
    val id = Mat.tabulate[Double, Three, Three](delta)
    (a   * id)   should equal (a)
    (id  * a)    should equal (a)
    (v.t * id).t should equal (v)
    (id  * v)    should equal (v)
    (a   * e1)   should equal (e2)

    val b1 = (a * a)
    b1 should equal (Mat.tabulate[Double, Three, Three](List[Double](
      0, 0, 0,
      2, 4, 0,
      0, 0, 0
    )))

    // Matrix multiplication seen as the inner product of vectors of
    // rows/columns with element-wise multiplication given by the
    // outer product of the rows-columns pairs.
    val b2 = a.t.asRowMajVec.t * a.asRowMajVec.map(_.t)
    b2.asScalar should equal (b1)
  }
}
