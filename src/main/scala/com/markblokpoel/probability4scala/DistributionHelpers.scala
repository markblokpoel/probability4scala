package com.markblokpoel.probability4scala

/** Helpers for [[Distribution]] and [[ConditionalDistribution]]. */
object DistributionHelpers {
  def pr[A](value: A, distribution: Distribution[A]): BigDecimal = distribution.pr(value)

  def pr[A, B](conditional: Conditional1[A, B], distribution: ConditionalDistribution[A, B]): BigDecimal =
    distribution.pr(conditional)

  def prV1[A, B](value: A, distribution: ConditionalDistribution[A, B]): BigDecimal = distribution.prV1(value)

  def pr[A, B](given: B, distribution: ConditionalDistribution[A, B]): Distribution[A] = distribution.pr(given)
}
