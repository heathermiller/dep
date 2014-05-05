package ch.epfl.lamp.dep
package arith

import scala.reflect.ClassTag

import org.scalatest.{ FlatSpecLike, Matchers }

import internal.Proof


class Vectors {

  class Vec[+T, N <: Pt[Int]] private (
    val length: Int, private[this] val data: Array[T]){

    protected def apply(n: Int): T = data(n)

    /** Coordinate projection. */
    def apply[M <: Pt[Int]](
      implicit mip: Interpreter[Int, M],
      pf: Proof[(Zero <= M) && (M < N)]): T = {
      data(interpret[Int, M])
    }

    /** Scalar vector sum. */
    def +[S >: T, M <: Pt[Int]](v: Vec[S, M])(
      implicit add: Operator_+[S, S, S], tt: ClassTag[S], eq: N =*= M): Vec[S, N] = {
      val newData = Array.tabulate[S](length) { n => add(data(n), v(n)) }
      new Vec[S, N](length, newData)
    }

    /** Scalar multiplication. */
    def *[S >: T](x: S)(
      implicit mul: Operator_*[S, S, S], tt: ClassTag[S]): Vec[S, N] = {
      val newData = Array.tabulate[S](length) { n => mul(data(n), x) }
      new Vec[S, N](length, newData)
    }

    /** Scalar/dot product. */
    def *[S >: T, M <: Pt[Int]](v: Vec[S, M])(
      implicit add: Operator_+[S, S, S], mul: Operator_*[S, S, S], eq: N =*= M,
      pf: Proof[N > Zero]): S = {
      var r = mul(v(0), data(0))
      (1 until length) foreach { i => r = add(r, mul(v(i), data(i))) }
      r
    }
  }

  object Vec {

    /** Factory method. */
    def tabulate[T, N <: Pt[Int]](f: Int => T)(
      implicit tt: ClassTag[T], nip: Interpreter[Int, N],
      pf: Proof[N >= Zero]): Vec[T, N] = {
      val n = interpret[Int, N]
      new Vec[T, N](n, Array.tabulate[T](n)(f))
    }
  }
}

class VecSpec extends Vectors with FlatSpecLike with Matchers {

  // Shortcuts
  type One   = Pt[Int@lit(1)]
  type Two   = Pt[Int@lit(2)]
  type Three = Pt[Int@lit(3)]

  it should "do some vector algebra" in {

    // 3D coordinate base
    val delta: Int => Int => Double = { x => y => if (x == y) 1 else 0 }
    val e1 = Vec.tabulate[Double, Three](delta(0))
    val e2 = Vec.tabulate[Double, Three](delta(1))
    val e3 = Vec.tabulate[Double, Three](delta(2))

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

    // Scalar/dot products
    (e1 * e2) should equal (0)
    (v  * e2) should equal (v[One])
  }
}
