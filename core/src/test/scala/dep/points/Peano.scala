package ch.epfl.lamp.dep

import org.scalatest.FlatSpecLike
import org.scalatest.Matchers

import points._

class Peano {

  // Shortcuts
  type One   = Succ[Zero]
  type Two   = Succ[One]
  type Three = Succ[Two]
}

class PeanoSpec extends Peano with FlatSpecLike with Matchers {

  it should "interpret some arithmetic correctly" in {

    // Basics
    interpret[Int, Zero ] should equal(0)
    interpret[Int, One  ] should equal(1)
    interpret[Int, Two  ] should equal(2)
    interpret[Int, Three] should equal(3)

    // Sums
    interpret[Int, Zero + One ] should equal(1)
    interpret[Int, Two + Three] should equal(5)

    // Differences
    interpret[Int, One - Zero]   should equal(1)
    interpret[Int, Three - Two]  should equal(1)
    interpret[Int, Zero - Three] should equal(-3)

    // Products
    interpret[Int, *[One, Zero]]  should equal(0)
    interpret[Int, *[Three, Two]] should equal(6)
    interpret[Int, *[One, Three]] should equal(3)

    // Quotients
    interpret[Int, Zero / Two] should equal(0)
    interpret[Int, Two / One]  should equal(2)
    interpret[Int, Two / Two]  should equal(1)
  }

  it should "prove some arithmetic assertions" in {

    // Basics
    prove[Zero === Zero]
    prove[One  === One]

    // Sums
    prove[Zero + One === One]

    // Differences
    prove[One - Zero  === One]
    prove[Three - Two === One]

    // Products
    prove[*[One, Zero]  === Zero]
    prove[*[One, Three] === Three]

    // Quotients
    prove[Zero / Two === Zero]
    prove[Two / One  === Two]
    prove[Two / Two  === One]
  }
}
