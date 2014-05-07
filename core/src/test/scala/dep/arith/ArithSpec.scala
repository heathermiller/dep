package ch.epfl.lamp.dep
package arith

import org.scalatest.FlatSpecLike
import org.scalatest.Matchers


class ArithSpec extends FlatSpecLike with Matchers {

  // Shortcuts
  type One   = Int@lit(1)
  type Two   = Int@lit(2)
  type Three = Int@lit(3)

  it should "interpret some arithmetic correctly" in {

    // Basics
    interpret[Int, Zero ] should equal(0)
    interpret[Int, One  ] should equal(1)
    interpret[Int, Two  ] should equal(2)
    interpret[Int, Three] should equal(3)
    // FIXME: An extra application of type-level identity function
    // `Id` is necessary here because the implicit materializer seems
    // to drop the (top-level) type annotation on its type arguments.
    // By Adding an extra "layer" of the `Id` type, the annotation is
    // preserved.  This is probably a bug in the compiler...
    interpret[Int, Id[Int@lit(123)]] should equal(123)
    interpret[Double, Id[Double@lit(3.0)]] should equal(3.0)

    // Sums
    interpret[Int, Zero + One ] should equal(1)
    interpret[Int, Two + Three] should equal(5)
    interpret[Int, Int@lit(123) + Two] should equal(125)
    interpret[Double, Double@lit(2.0) + Double@lit(3.0)] should equal(5.0)

    // Differences
    interpret[Int, One - Zero]   should equal(1)
    interpret[Int, Three - Two]  should equal(1)
    interpret[Int, Zero - Three] should equal(-3)
    interpret[Int, Int@lit(123) - Two] should equal(121)

    // Products
    interpret[Int, *[One, Zero]]  should equal(0)
    interpret[Int, *[Three, Two]] should equal(6)
    interpret[Int, *[One, Three]] should equal(3)
    interpret[Int, *[Int@lit(44), Two]] should equal(88)

    // Quotients
    interpret[Int, Zero / Two] should equal(0)
    interpret[Int, Two / One]  should equal(2)
    interpret[Int, Two / Two]  should equal(1)
    interpret[Int, Int@lit(16) / Two]  should equal(8)

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
    prove[Zero == Zero]
    prove[One  == One]
    prove[Int@lit(123) == Int@lit(123)]
    prove[Double@lit(3.0) == Double@lit(3.0)]
    prove[Three == Double@lit(3.0)]

    // Sums
    prove[Zero + One == One]
    prove[Int@lit(123) + Two == Int@lit(125)]
    prove[Double@lit(2.0) + Double@lit(3.0) == Double@lit(5.0)]

    // Differences
    prove[One - Zero  == One]
    prove[Three - Two == One]
    prove[Int@lit(123) - Two == Int@lit(121)]

    // Products
    prove[*[One, Zero]  == Zero]
    prove[*[One, Three] == Three]
    prove[*[Int@lit(44), Two] == Int@lit(88)]

    // Quotients
    prove[Zero / Two == Zero]
    prove[Two / One  == Two]
    prove[Two / Two  == One]
    prove[Int@lit(16) / Two == Int@lit(8)]

    // Comparisons
    prove[Zero < Two]
    prove[Zero <= Two]
    prove[Zero <= Zero]
    prove[Two > Zero]
    prove[Two >= Zero]
    prove[Zero >= Zero]
  }
}
