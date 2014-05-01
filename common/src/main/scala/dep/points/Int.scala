package ch.epfl.lamp.dep
package points

// Peano-encoded natural numbers and their interpreters.

// FIXME: Replace be a more scalable representation.

/** Type-level zero literal. */
sealed trait Zero extends Pt[Int]
object Zero {
  implicit object Eval extends Interpreter[Int, Zero] {
    val value = 0
  }
}

/** Type-level successor operator. */
sealed trait Succ[N <: Pt[Int]] extends Pt[Int]
object Succ {
  implicit def eval[N <: Pt[Int]](implicit pred: Interpreter[Int, N]) =
    new Interpreter[Int, Succ[N]] { val value = pred.value + 1 }
}

// Arithmetic operators.

// FIXME: Remove all this boiler plate.  Generate these traits and
// their interpreters using a macro annotation.

sealed trait +[M <: Pt[Int], N <: Pt[Int]] extends Pt[Int]
object + {
  implicit def eval[M <: Pt[Int], N <: Pt[Int]](
    implicit mip: Interpreter[Int, M], nip: Interpreter[Int, N]) =
    new Interpreter[Int, M + N] { val value = mip.value + nip.value  }
}
sealed trait -[M <: Pt[Int], N <: Pt[Int]] extends Pt[Int]
object - {
  implicit def eval[M <: Pt[Int], N <: Pt[Int]](
    implicit mip: Interpreter[Int, M], nip: Interpreter[Int, N]) =
    new Interpreter[Int, M - N] { val value = mip.value - nip.value  }
}
sealed trait *[M <: Pt[Int], N <: Pt[Int]] extends Pt[Int]
object * {
  implicit def eval[M <: Pt[Int], N <: Pt[Int]](
    implicit mip: Interpreter[Int, M], nip: Interpreter[Int, N]) =
    new Interpreter[Int, *[M, N]] { val value = mip.value * nip.value  }
}
sealed trait /[M <: Pt[Int], N <: Pt[Int]] extends Pt[Int]
object / {
  implicit def eval[M <: Pt[Int], N <: Pt[Int]](
    implicit mip: Interpreter[Int, M], nip: Interpreter[Int, N]) =
    new Interpreter[Int, M / N] { val value = mip.value / nip.value  }
}
