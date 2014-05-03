package ch.epfl.lamp.dep

import scala.reflect.ClassTag

import org.scalatest.FlatSpecLike
import org.scalatest.Matchers

import points._


class Vectors {

  class Vec[+T, N <: Pt[Int]] private (private[this] val data: Array[T])(
    implicit nip: Interpreter[Int, N]){

    val length = interpret[Int, N]

    def apply[M <: Pt[Int]](
      implicit nip: Interpreter[Int, M]): T = {
      val n = interpret[Int, M]
      data(n)
    }
  }
  object Vec {
    def init[T, N <: Pt[Int]](f: Int => T)(
      implicit tt: ClassTag[T], nip: Interpreter[Int, N]): Vec[T, N] = {
      val n = interpret[Int, N]
      new Vec[T, N](Array.tabulate[T](n)(f))
    }
  }
}

class VecSpec extends Vectors with FlatSpecLike with Matchers {

  // Shortcuts
  type One   = Cst[Int@lit(1)]
  type Two   = Cst[Int@lit(2)]
  type Three = Cst[Int@lit(3)]

  it should "do some vector algebra" in {

    // 3D coordinate base
    val delta: Int => Int => Double = { x => y => if (x == y) 1 else 0 }
    val e1 = Vec.init[Double, Three](delta(0))
    val e2 = Vec.init[Double, Three](delta(1))
    val e3 = Vec.init[Double, Three](delta(2))

    e1.length should equal(3)
    e2.length should equal(3)
    e2.length should equal(3)

    e1[Zero] should equal(1)
    e1[One] should equal(0)
    e1[Two] should equal(0)
  }
}
