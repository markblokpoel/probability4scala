package com.markblokpoel.probability4scala

import Implicits._

import scala.reflect.ClassTag

/**
 * Implements a generic conditional probability distribution for two variables: {{{pr(V1|V2)}}}.
 * The probabilities are represented by BigDecimal for increased accuracy.
 * @param domainV1 The domain of variable 1 {{{V1}}}.
 * @param domainV2 The domain of variable 2 {{{V2}}}.
 * @param distribution The probabilities for each value in the domain.
 * @tparam A The type of variable 1.
 * @tparam B The type of variable 2.
 */
case class ConditionalDistribution[A, B](domainV1: Set[A], domainV2: Set[B], distribution: Map[(A, B), BigDecimal]) {
  /**
   * Recommended usage: {{{pr(a | b}}}.
   * @param conditional
   * @return The probability of the conditional.
   */
  @throws[NoSuchElementException]
  def pr(conditional: Conditional1[A, B]): BigDecimal = distribution((conditional.value, conditional.condition))

  /**
   * Computes the posterior probability [[Distribution]] given the contional value. Normalized by the marginal likelihood.
   * @param conditional
   * @return
   */
  def pr(conditional: B): Distribution[A] = this * domainV2.singleValueDistribution(conditional) / prV2(conditional)

  /**
   * Computes the probability for {{{V1=value}}} by marginalizing over all possible values of {{{V2}}}.
   * @param value
   * @return
   */
  def prV1(value: A): BigDecimal = domainV2.foldLeft(0.toBigDecimal) {
    (acc: BigDecimal, conditional: B) => acc + pr(value | conditional)
  }

  /**
   * Computes the probability for {{{V2=conditional}}} by marginalizing over all possible values of {{{V1}}}.
   * @param conditional
   * @return
   */
  def prV2(conditional: B): BigDecimal = domainV1.foldLeft(0.toBigDecimal) {
    (acc: BigDecimal, value: A) => acc + pr(value | conditional)
  }

  /**
   * Computes the marginal probability for all values of {{{V1}}} by marginalizing over all possible values of {{{V2}}}.
   * @return
   */
  def marginalDistribution: Distribution[A] =
    Distribution(domainV1, domainV1.map(value => value -> prV1(value)).toMap)

  /**
   * Computes the marginal probability for all values of {{{V2}}} by marginalizing over all possible values of {{{V1}}}.
   * @return
   */
  def marginalLikelihood: Distribution[B] =
    Distribution(domainV2, domainV2.map(value => value -> prV2(value)).toMap)

  /**
   * Computes the de-normalized posterior distribution of {{{V1}}} given a priors distribution over {{{V2}}}.
   * @param prior
   * @return
   */
  def *(prior: Distribution[B]): Distribution[A] = Distribution(domainV1,
    domainV1.map((value1: A) =>
      value1 -> domainV2.map((value2: B) => pr(value1 | value2) * prior.pr(value2)).sum
    ).toMap
  )

  /**
   * Computes the posterior probability given a value for {{{V1}}} and a prior distribution over {{{V2}}}
   * via conditionalization. Recommended usage: {{{pr(v1 | Distribution(V2))}}}
   * @param conditional
   * @return
   */
  def pr(conditional: Conditional2[A, B]): BigDecimal =
    domainV2.map(condition => pr(conditional.value | condition) * conditional.prior.pr(condition)).sum

  /**
   * Computes the inverse distribution using Bayes' rule given a prior distribution over {{{V2}}}. The
   * distribution is normalized using the marginal likelihood.
   * @param prior
   * @return
   */
  def bayes(prior: Distribution[B]): ConditionalDistribution[B, A] = {
    val newDistribution: Set[((B, A), BigDecimal)] = domainV2.flatMap(condition => {
      domainV1.map(value => {
        (condition, value) -> pr(value | condition) * prior.pr(condition) / prV2(condition)
      })
    })
    ConditionalDistribution(domainV2, domainV1, newDistribution.toMap)
  }

  /**
   * @return The sum of the probabilities, i.e., the probability mass.
   */
  def sum: BigDecimal = distribution.values.sum

  /** Prints the distribution as a conditional probability table. */
  def cpt(): Unit = {
    val maxStrLen1 = domainV1.map(_.toString.length).max
    val maxStrLen2 = math.max(6, domainV2.map(_.toString.length).max)
    println(" " * (maxStrLen1+1) + domainV2.map(d2 => d2.toString + " " * (maxStrLen2 - d2.toString.length + 1)).mkString)
    domainV1.foreach { d1 =>
      println(
        d1.toString + " " * (maxStrLen1 - d1.toString.length + 1) +
          domainV2.toVector.map(d2 => f"${pr(d1 | d2)}%1.4f" + " " * (maxStrLen2 - f"${pr(d1 | d2)}%1.4f".length + 1)).mkString
      )
    }
  }
}

/** Factory for [[ConditionalDistribution]]. */
case object ConditionalDistribution {
  def apply[A, B, X: ClassTag](domain1: Set[A], domain2: Set[B], distribution: Map[(A, B), Double]): ConditionalDistribution[A, B] = {
    val bdd = distribution.mapValues(BigDecimal(_))
    ConditionalDistribution[A, B](domain1, domain2, bdd)
  }
}

