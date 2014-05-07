package ch.epfl.lamp.dep

import org.scalatest.FlatSpecLike
import org.scalatest.Matchers

import arith._

class TypeOpsSpec extends FlatSpecLike with Matchers {

  it should "interpret some type-level programs correctly" in {

    // Functions
    interpret[Int,
      // (x: Int => x)(2)
      (Int@v("x") => Int@v("x")) @@ Int@lit(2)
    ] should equal(2)
    interpret[Int,
      // (x: Int => 1 + x)(2)
      (Int@v("x") => Int@lit(1) + Int@v("x")) @@ Int@lit(2)
    ] should equal(3)

    // Variable binding
    interpret[Int,
      // val x = 0 + 1; x
      Int@v("x") := (Zero + Int@lit(1)) in
        Int@v("x")
    ] should equal(1)
    interpret[Int,
      // val x = 0; x + 1
      Int@v("x") := Zero in
        Int@v("x") + Int@lit(1)
    ] should equal(1)
  }
}
