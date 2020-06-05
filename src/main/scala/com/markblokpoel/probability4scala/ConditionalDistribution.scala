package com.markblokpoel.probability4scala

import Implicits._
import com.markblokpoel.probability4scala.datastructures.BigNatural

import scala.reflect.ClassTag

/**
 * Implements a generic conditional probability distribution for two variables: {{{pr(V1|V2)}}}.
 * The probabilities are represented by BigNatural for increased accuracy.
 * @param domainV1 The domain of variable 1 {{{V1}}}.
 * @param domainV2 The domain of variable 2 {{{V2}}}.
 * @param distribution The probabilities for each value in the domain.
 * @tparam A The type of variable 1.
 * @tparam B The type of variable 2.
 */
case class ConditionalDistribution[A, B](domainV1: Set[A], domainV2: Set[B], distribution: Map[(A, B), BigNatural]) {
  /**
   * Recommended usage: {{{pr(a | b}}}.
   * @param conditional
   * @return The probability of the conditional.
   */
  @throws[NoSuchElementException]
  def pr(conditional: Conditional1[A, B]): BigNatural = distribution((conditional.value, conditional.condition))

  /**
   * Computes the posterior probability [[Distribution]] given the contional value. Normalized by the marginal likelihood.
   * @param conditional
   * @return
   */
  def pr(conditional: B): Distribution[A] = {
    val d = this * domainV2.singleValueDistribution(conditional)
    if(d.sum == 0) d
    else this * domainV2.singleValueDistribution(conditional) / prConditional(conditional)
  }

  /**
   * Computes the probability for {{{V1=value}}} by marginalizing over all possible values of {{{V2}}}.
   * @param value
   * @return
   */
  def prValue(value: A): BigNatural = domainV2.foldLeft(0.toBigNatural) {
    (acc: BigNatural, conditional: B) => acc + pr(value | conditional)
  }

  /**
   * Computes the probability for {{{V2=conditional}}} by marginalizing over all possible values of {{{V1}}}.
   * @param conditional
   * @return
   */
  def prConditional(conditional: B): BigNatural = domainV1.foldLeft(0.toBigNatural) {
    (acc: BigNatural, value: A) => acc + pr(value | conditional)
  }

  /**
   * Computes the marginal probability for all values of {{{V1}}} by marginalizing over all possible values of {{{V2}}}.
   * @return
   */
  def marginalDistribution: Distribution[A] =
    Distribution(domainV1, domainV1.map(value => value -> prValue(value)).toMap)

  /**
   * Computes the marginal probability for all values of {{{V2}}} by marginalizing over all possible values of {{{V1}}}.
   * @return
   */
  def marginalLikelihood: Distribution[B] =
    Distribution(domainV2, domainV2.map(value => value -> prConditional(value)).toMap)

  /**
   * Computes the de-normalized posterior distribution of {{{V1}}} given a priors distribution over {{{V2}}}.
   * @param prior
   * @return
   */
  def *(prior: Distribution[B]): Distribution[A] = Distribution(domainV1,
    domainV1.map((value1: A) =>
      value1 ->
        domainV2
          .map((value2: B) => pr(value1 | value2) * prior.pr(value2))
          .fold(BigNatural(0))((acc: BigNatural, p: BigNatural) => acc + p)
    ).toMap
  )

  def --(other: Distribution[A]): ConditionalDistribution[A, B] = {
    require(domainV1 == other.domain, "subtraction for distributions requires the same domains")

    val newDistribution = (for(d1 <- domainV1; d2 <- domainV2) yield
      ((d1, d2) -> (distribution((d1, d2)) - other.distribution(d1)))).toMap

    ConditionalDistribution(domainV1, domainV2, newDistribution)
  }

  def ---(other: Distribution[B]): ConditionalDistribution[A, B] = {
    require(domainV2 == other.domain, "subtraction for distributions requires the same domains")

    val newDistribution = (for(d1 <- domainV1; d2 <- domainV2) yield
      ((d1, d2) -> (distribution((d1, d2)) - other.distribution(d2)))).toMap

    ConditionalDistribution(domainV1, domainV2, newDistribution)
  }

  /**
   * Scales the conditional distribution according to the scalar: pr(domainV1 | domainV2) * scalar
   *
   * This may de-normalize the distribution.
   *
   * @param scalar
   * @return The scaled distribution.
   */
  def *(scalar: BigNatural): ConditionalDistribution[A, B] =
    ConditionalDistribution(domainV1, domainV2, distribution.mapValues(_ * scalar))

  /**
   * Inversely scales the distribution according to a scalar: pr(domain) * 1 / scalar = pr(domain) / scalar
   *
   * This may de-normalize the distribution.
   *
   * @param scalar A scalar bigger than 0.
   * @return The scaled distribution.
   */
  @throws[IllegalArgumentException]
  def /(scalar: BigNatural): ConditionalDistribution[A, B] = {
    require(scalar != 0, "Cannot divide by 0.")
    ConditionalDistribution(domainV1, domainV2, distribution.mapValues(_ / scalar))
  }

  def /(other: Distribution[B]): ConditionalDistribution[A, B] = {
    val divDistribution = (for(value1 <- domainV1; value2 <- domainV2) yield {
      val nominator = distribution((value1, value2))
      val denominator = other.distribution(value2)
      if(nominator == 0) (value1, value2) -> 0.0.toBigNatural
      else (value1, value2) -> (nominator / denominator)
    }).toMap
    ConditionalDistribution(domainV1, domainV2, divDistribution)
  }

  def +(other: ConditionalDistribution[A, B]): ConditionalDistribution[A, B] = {
    val sumDistribution = (for(value1 <- domainV1; value2 <- domainV2) yield {
      (value1, value2) -> (distribution((value1, value2)) + other.distribution((value1, value2)))
    }).toMap
    ConditionalDistribution(domainV1, domainV2, sumDistribution)
  }

  /**
   * Computes the posterior probability given a value for {{{V1}}} and a prior distribution over {{{V2}}}
   * via conditionalization. Recommended usage: {{{pr(v1 | Distribution(V2))}}}
   * @param conditional
   * @return
   */
  def pr(conditional: Conditional2[A, B]): BigNatural =
    domainV2.map(condition => pr(conditional.value | condition) * conditional.prior.pr(condition))
      .fold(BigNatural(0))((acc: BigNatural, p: BigNatural) => acc + p)

  /**
   * Computes the inverse distribution using Bayes' rule given a prior distribution over {{{V2}}}. The
   * distribution is not normalized.
   * @param prior
   * @return
   */
  def bayes(prior: Distribution[B]): ConditionalDistribution[B, A] = {
    val newDistribution: Set[((B, A), BigNatural)] = domainV2.flatMap(condition => {
      domainV1.map(value => {
        val numerator = pr(value | condition) * prior.pr(condition)
//        val denominator = prConditional(condition)
//        if(denominator == 0) (condition, value) -> BigNatural(0)
        (condition, value) -> numerator// / denominator
      })
    })
    ConditionalDistribution(domainV2, domainV1, newDistribution.toMap)
  }

  def isNormalized: Boolean = sum == 1

  def accuracy: BigNatural = BigNatural(1.0) - sum

  /** Returns the Shannon information conditional entropy of this distribution.
   *
   * For distributions that deviate from probability assumptions (i.e., the sum of the values
   * equals 1.0), Shannon information entropy is ill-defined.
   *
   * @return Conditional entropy of the distribution
   */
//  def entropy: Double =
//    (for(a <- domainV1; b <- domainV2) yield {
//      pr(a | b).doubleValue * math.log((pr(a | b) / prV2(b)).doubleValue)
//    }).sum

  /**
   * @return The sum of the probabilities, i.e., the probability mass.
   */
  def sum: BigNatural = distribution.values.fold(BigNatural(0))((acc: BigNatural, p: BigNatural) => acc + p)

  def exp: ConditionalDistribution[A, B] = ConditionalDistribution(domainV1, domainV2, distribution.mapValues(_.exp))

  def log: ConditionalDistribution[A, B] = ConditionalDistribution(domainV1, domainV2, distribution.mapValues(_.log))

  /**
   * Returns the softmaxed distribution
   *
   * @param beta The beta parameter
   * @return A value in the domain of distribution
   * @see See this Wikipedia page for a mathmatical definition of soft argmax
   *      [[https://en.wikipedia.org/wiki/Softmax_function]].
   */
  def softmax(beta: BigNatural): ConditionalDistribution[A, B] =
    softmax(beta, domainV1.nilDistribution)

  /**
   * Returns the softmaxed distribution
   *
   * @param beta The beta parameter
   * @param costs The cost function as distribution (doesn't have to be normalized)
   * @return A value in the domain of distribution
   * @see See this Wikipedia page for a mathmatical definition of soft argmax
   *      [[https://en.wikipedia.org/wiki/Softmax_function]].
   */
  def softmax(beta: BigNatural, costs: Distribution[A]): ConditionalDistribution[A, B] = {
    val softened = (this.log * beta -- costs).exp
//    println("soft")
//    println(softened.distribution)
//    for(v <- domainV1; c <- domainV2)
//     println(v + "|" + c + " = " + softened.pr(v | c))

//    val norm = (for(c <- domainV2) yield {
//      (c -> softened.pr(c).sum)
//    }).toMap

    val norm = softened.distribution.values.fold(0.toBigNatural)(_ + _)

//    println("norm")
//    println(norm)

    val newDistr = (for(v <- domainV1; c <- domainV2) yield {
      (v, c) -> softened.pr(v | c) / norm
    }).toMap

    ConditionalDistribution(domainV1, domainV2, newDistr)
  }

  /** Prints the distribution as a conditional probability table. */
  def cpt(): Unit = {
    val maxStrLen1 = domainV1.map(_.toString.length).max
    val maxStrLen2 = math.max(6, domainV2.map(_.toString.length).max)
    println(" " * (maxStrLen1+1) + domainV2.map(d2 => d2.toString + " " * (maxStrLen2 - d2.toString.length + 1)).mkString)
    domainV1.foreach { d1 =>
      println(
        d1.toString + " " * (maxStrLen1 - d1.toString.length + 1) +
          domainV2.toVector.map(d2 => f"${pr(d1 | d2).doubleValue()}%1.4f" + " " * (maxStrLen2 - f"${pr(d1 | d2).doubleValue()}%1.4f".length + 1)).mkString
      )
    }
  }
}

/** Factory for [[ConditionalDistribution]]. */
case object ConditionalDistribution {
  def apply[A, B, X: ClassTag](domain1: Set[A], domain2: Set[B], distribution: Map[(A, B), Double]): ConditionalDistribution[A, B] = {
    val bdd = distribution.mapValues(BigNatural(_))
    ConditionalDistribution[A, B](domain1, domain2, bdd)
  }
}

