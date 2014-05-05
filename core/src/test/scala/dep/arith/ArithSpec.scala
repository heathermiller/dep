package ch.epfl.lamp.dep
package arith

import org.scalatest.FlatSpecLike
import org.scalatest.Matchers


class ArithSpec extends FlatSpecLike with Matchers {

  // Shortcuts
  type One   = Pt[Int@lit(1)]
  type Two   = Pt[Int@lit(2)]
  type Three = Pt[Int@lit(3)]

  it should "interpret some arithmetic correctly" in {

    // Basics
    interpret[Int, Zero ] should equal(0)
    interpret[Int, One  ] should equal(1)
    interpret[Int, Two  ] should equal(2)
    interpret[Int, Three] should equal(3)
    interpret[Int, Pt[Int@lit(123)]] should equal(123)
    interpret[Double, Pt[Double@lit(3.0)]] should equal(3.0)

    // Sums
    interpret[Int, Zero + One ] should equal(1)
    interpret[Int, Two + Three] should equal(5)
    interpret[Int, Pt[Int@lit(123)] + Two] should equal(125)
    interpret[Double, Pt[Double@lit(2.0)] + Pt[Double@lit(3.0)]] should equal(5.0)

    // Differences
    interpret[Int, One - Zero]   should equal(1)
    interpret[Int, Three - Two]  should equal(1)
    interpret[Int, Zero - Three] should equal(-3)
    interpret[Int, Pt[Int@lit(123)] - Two] should equal(121)

    // Products
    interpret[Int, *[One, Zero]]  should equal(0)
    interpret[Int, *[Three, Two]] should equal(6)
    interpret[Int, *[One, Three]] should equal(3)
    interpret[Int, *[Pt[Int@lit(44)], Two]] should equal(88)

    // Quotients
    interpret[Int, Zero / Two] should equal(0)
    interpret[Int, Two / One]  should equal(2)
    interpret[Int, Two / Two]  should equal(1)
    interpret[Int, Pt[Int@lit(16)] / Two]  should equal(8)

    // Comparisons
    interpret[Boolean, Zero < Two]   should equal(true)
    interpret[Boolean, Two < Zero]   should equal(false)
    interpret[Boolean, Zero < Zero]  should equal(false)
    interpret[Boolean, Zero <= Two]  should equal(true)
    interpret[Boolean, Two <= Zero]  should equal(false)
    interpret[Boolean, Zero <= Zero] should equal(true)
    interpret[Boolean, Zero > Two]   should equal(false)
    interpret[Boolean, Two > Zero]   should equal(true)
    interpret[Boolean, Zero > Zero]  should equal(false)
    interpret[Boolean, Zero >= Two]  should equal(false)
    interpret[Boolean, Two >= Zero]  should equal(true)
    interpret[Boolean, Zero >= Zero] should equal(true)
  }

  it should "prove some arithmetic assertions" in {

    // Basics
    prove[Zero ==! Zero]
    prove[One  ==! One]
    prove[Pt[Int@lit(123)] ==! Pt[Int@lit(123)]]
    prove[Pt[Double@lit(3.0)] ==! Pt[Double@lit(3.0)]]
    prove[Three ==! Pt[Double@lit(3.0)]]

    // Sums
    prove[Zero + One ==! One]
    prove[Pt[Int@lit(123)] + Two ==! Pt[Int@lit(125)]]
    prove[Pt[Double@lit(2.0)] + Pt[Double@lit(3.0)] ==! Pt[Double@lit(5.0)]]

    // Differences
    prove[One - Zero  ==! One]
    prove[Three - Two ==! One]
    prove[Pt[Int@lit(123)] - Two ==! Pt[Int@lit(121)]]

    // Products
    prove[*[One, Zero]  ==! Zero]
    prove[*[One, Three] ==! Three]
    prove[*[Pt[Int@lit(44)], Two] ==! Pt[Int@lit(88)]]

    // Quotients
    prove[Zero / Two ==! Zero]
    prove[Two / One  ==! Two]
    prove[Two / Two  ==! One]
    prove[Pt[Int@lit(16)] / Two ==! Pt[Int@lit(8)]]

    // Comparisons
    prove[Zero < Two]
    prove[Zero <= Two]
    prove[Zero <= Zero]
    prove[Two > Zero]
    prove[Two >= Zero]
    prove[Zero >= Zero]
  }
}
