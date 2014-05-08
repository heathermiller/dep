package ch.epfl.lamp.dep

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.Type

import org.scalatest.FlatSpecLike
import org.scalatest.Matchers

import arith._
import internal.Proof


class PiSigmaSpec
    extends VectorsAndMatrices
    with FlatSpecLike
    with Matchers {

  // Shortcuts
  type Two  = Int@lit(2)
  type Four = Int@lit(4)
  type Vec[T, N] = Mat[T, N, One]

  it should "use some dependent types correctly" in {

    // Pi-types

    // fillVec: Pi(T: Type).Pi(n: Nat).Pi(x: T).Vec[T, n]
    def fillVec[T, N, X](
      implicit e1: T :~ Type, e2: N :~ Int,  e3: X :~ T,
      pf: Proof[N >= Zero], tt: ClassTag[T]): Vec[T, N] =
      Vec.tabulate[T, N] { _ => interpret[T, X] }

    val v = fillVec[Double, Four, Id[Double@lit(1.23)]]
    v should equal(Mat.tabulate[Double, Four, One](List(
      1.23, 1.23, 1.23, 1.23
    )))


    // Sigma-types

    // PairLengthVec: Pi(T: Type).Sigma(n: Nat).Vec[T, 2 * n]
    class PairLengthVec[T, N, M](val _2: Vec[T, M])(
      implicit e1: T :~ Type, e2: N :~ Int, eq: M =*= *[Two, N],
      pf: Proof[N >= Zero], tt: ClassTag[T]) {
      type _1 = N
      val _1: Int = interpret[Int, N]
    }

    val plv = new PairLengthVec[Double, Two, v.Height](v)
    plv._1 should equal(2)
    plv._2 should equal(v)
    plv._2.length should equal(4)
  }
}
