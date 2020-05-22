package com.markblokpoel.probability4scala

import com.markblokpoel.probability4scala.datastructures.BigNatural

/** Helpers for [[Distribution]] and [[ConditionalDistribution]]. */
object DistributionHelpers {
  def pr[A](value: A, distribution: Distribution[A]): BigDecimal = distribution.pr(value)

  def pr[A, B](conditional: Conditional1[A, B], distribution: ConditionalDistribution[A, B]): BigDecimal =
    distribution.pr(conditional)

  def prValue[A, B](value: A, distribution: ConditionalDistribution[A, B]): BigDecimal = distribution.prValue(value)

  def pr[A, B](given: B, distribution: ConditionalDistribution[A, B]): Distribution[A] = distribution.pr(given)

  def exp[A](distribution: Distribution[A]): Distribution[A] = distribution.exp

  def log[A](distribution: Distribution[A]): Distribution[A] = distribution.log

  def exp[A, B](distribution: ConditionalDistribution[A, B]): ConditionalDistribution[A, B] = distribution.exp

  def log[A, B](distribution: ConditionalDistribution[A, B]): ConditionalDistribution[A, B] = distribution.log

  def min(nat1: BigNatural, nat2: BigNatural): BigNatural = nat1 min nat2

  def max(nat1: BigNatural, nat2: BigNatural): BigNatural = nat1 max nat2

  def min(double: Double, nat: BigNatural): BigNatural = BigNatural(double) min nat

  def max(double: Double, nat: BigNatural): BigNatural = BigNatural(double) max nat

  def min(nat: BigNatural, double: Double): BigNatural = nat min double

  def max(nat: BigNatural, double: Double): BigNatural = nat max double
}
