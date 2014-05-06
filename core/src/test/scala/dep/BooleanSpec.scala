package ch.epfl.lamp.dep

import org.scalatest.FlatSpecLike
import org.scalatest.Matchers


class BooleanSpec extends FlatSpecLike with Matchers {

  it should "interpret some logical assertions correctly" in {

    // Basics
    interpret[Boolean, True ] should equal(true)
    interpret[Boolean, False] should equal(false)

    // Negations
    interpret[Boolean, ![True] ] should equal(false)
    interpret[Boolean, ![False]] should equal(true)

    // Conjunctions
    interpret[Boolean, True  && True ] should equal(true)
    interpret[Boolean, True  && False] should equal(false)
    interpret[Boolean, False && True ] should equal(false)
    interpret[Boolean, False && False] should equal(false)

    // Disjunctions
    interpret[Boolean, True  || True ] should equal(true)
    interpret[Boolean, True  || False] should equal(true)
    interpret[Boolean, False || True ] should equal(true)
    interpret[Boolean, False || False] should equal(false)

    // Equalities
    interpret[Boolean, True  == True ] should equal(true)
    interpret[Boolean, True  == False] should equal(false)
    interpret[Boolean, False == True ] should equal(false)
    interpret[Boolean, False == False] should equal(true)
  }

  it should "prove some logical assertions" in {

    // Basics
    prove[True]

    // Negations
    prove[![False]]

    // Conjunctions
    prove[True  && True ]

    // Disjunctions
    prove[True  || True ]
    prove[True  || False]
    prove[False || True ]

    // Equalities
    prove[True  == True ]
    prove[False == False]
  }
}
