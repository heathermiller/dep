package ch.epfl.lamp.dep

import scala.annotation.StaticAnnotation


/**
 * Annotation class for type-level literals.
 *
 * Types annotated with instances of this class represent type-level
 * literals.
 *
 * Use case:
 * {{{
 *   import ch.epfl.lamp.dep._
 *
 *   interpret[Boolean, Boolean@lit(true)]  // returns `true`
 *   prove[Boolean@lit(true) === True]       // succeeds
 *   prove[Boolean@lit(true) === False]      // fails
 * }}}
 */
final class lit(x: Any) extends StaticAnnotation


/**
 * Annotation class for type-level variables.
 *
 * Types annotated with instances of this class represent type-level
 * variables.
 *
 * Use case:
 * {{{
 *   import ch.epfl.lamp.dep._
 *
 *   interpret[Int,
 *       Int@v("x") := Zero + Int@lit(1) in
 *       Int@v("x")
 *   ]                                      // returns `1`
 * }}}
 */
final class v(x: Any) extends StaticAnnotation


/**
 * Type-level variable binding.
 *
 * Use case:
 * {{{
 *   import ch.epfl.lamp.dep._
 *
 *   interpret[Int,
 *     Int@v("x") := Zero + Int@lit(1) in
 *     Int@v("x")
 *   ]                                      // returns `1`
 * }}}
 */
sealed trait :=[X, B]


/**
 * Type-level sequencing.
 *
 * Use case:
 * {{{
 *   import ch.epfl.lamp.dep._
 *
 *   interpret[Int,
 *     Int@v("x") := Zero + Int@lit(1) in
 *     Int@v("x")
 *   ]                                      // returns `1`
 * }}}
 */
sealed trait in[X, B]


/**
 * Type-level function application.
 *
 * Use case:
 * {{{
 *   import ch.epfl.lamp.dep._
 *
 *   interpret[Int,
 *     (Int@v("x") => Int@v("x")) @@ Int@lit(2)
 *   ]                                      // returns `2`
 * }}}
 */
sealed trait @@[F, X]
