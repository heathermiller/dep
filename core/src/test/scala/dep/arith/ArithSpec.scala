package ch.epfl.lamp.dep
package arith

import org.scalatest.FlatSpecLike
import org.scalatest.Matchers


class IntSpec extends FlatSpecLike with Matchers {

  // Shortcuts
  type One   = Cst[Int@lit(1)]
  type Two   = Cst[Int@lit(2)]
  type Three = Cst[Int@lit(3)]

  it should "interpret some arithmetic correctly" in {

    // Basics
    interpret[Int, Zero ] should equal(0)
    interpret[Int, One  ] should equal(1)
    interpret[Int, Two  ] should equal(2)
    interpret[Int, Three] should equal(3)
    interpret[Int, Cst[Int@lit(123)]] should equal(123)

    // Sums
    interpret[Int, Zero + One ] should equal(1)
    interpret[Int, Two + Three] should equal(5)
    interpret[Int, Cst[Int@lit(123)] + Two] should equal(125)

    // Differences
    interpret[Int, One - Zero]   should equal(1)
    interpret[Int, Three - Two]  should equal(1)
    interpret[Int, Zero - Three] should equal(-3)
    interpret[Int, Cst[Int@lit(123)] - Two] should equal(121)

    // Products
    interpret[Int, *[One, Zero]]  should equal(0)
    interpret[Int, *[Three, Two]] should equal(6)
    interpret[Int, *[One, Three]] should equal(3)
    interpret[Int, *[Cst[Int@lit(44)], Two]] should equal(88)

    // Quotients
    interpret[Int, Zero / Two] should equal(0)
    interpret[Int, Two / One]  should equal(2)
    interpret[Int, Two / Two]  should equal(1)
    interpret[Int, Cst[Int@lit(16)] / Two]  should equal(8)
  }

  it should "prove some arithmetic assertions" in {

    // Basics
    prove[Zero === Zero]
    prove[One  === One]
    prove[Cst[Int@lit(123)] === Cst[Int@lit(123)]]

    // Sums
    prove[Zero + One === One]
    prove[Cst[Int@lit(123)] + Two === Cst[Int@lit(125)]]

    // Differences
    prove[One - Zero  === One]
    prove[Three - Two === One]
    prove[Cst[Int@lit(123)] - Two === Cst[Int@lit(121)]]

    // Products
    prove[*[One, Zero]  === Zero]
    prove[*[One, Three] === Three]
    prove[*[Cst[Int@lit(44)], Two] === Cst[Int@lit(88)]]

    // Quotients
    prove[Zero / Two === Zero]
    prove[Two / One  === Two]
    prove[Two / Two  === One]
    prove[Cst[Int@lit(16)] / Two === Cst[Int@lit(8)]]
  }
}
