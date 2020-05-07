package com.markblokpoel.probability4scala

object Helpers {
  def pr[A](value: A, distribution: Distribution[A]): BigDecimal = distribution.pr(value)

  def pr[A, B](conditional: Conditional[A, B], distribution: ConditionalDistribution[A, B]): BigDecimal =
    distribution.pr(conditional)

  def prA[A, B](value: A, distribution: ConditionalDistribution[A, B]): BigDecimal = distribution.prA(value)

  def pr[A, B](given: B, distribution: ConditionalDistribution[A, B]): Distribution[A] = distribution.pr(given)
}
