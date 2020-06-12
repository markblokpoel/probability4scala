package com.markblokpoel.probability4scala

import com.markblokpoel.probability4scala.datastructures.{BigNatural, ProbabilityTree}
import com.markblokpoel.probability4scala.Implicits._

import scala.reflect.ClassTag
import scala.util.Random

/**
 * Implements a generic probability distribution. The probabilities are represented by BigNatural for increased
 * accuracy.
 *
 * @param domain       The set of values that span this distribution.
 * @param distribution The probabilities for each value in the domain.
 * @tparam A The type of the domain.
 */
case class Distribution[A](domain: Set[A], distribution: Map[A, BigNatural]) {

  /**
   * @param value A value within the domain.
   * @return The probability of the value.
   */
  @throws[NoSuchElementException]
  def pr(value: A): BigNatural = distribution(value)

  /**
   * Scales the distribution according to the scalar: pr(domain) * scalar
   *
   * This may de-normalize the distribution.
   *
   * @param scalar
   * @return The scaled distribution.
   */
  def *(scalar: BigNatural): Distribution[A] = Distribution(domain, distribution.mapValues(_ * scalar))

  def +(other: Distribution[A]): Distribution[A] = {
    require(domain == other.domain, "addition for distributions requires the same domains")

    Distribution(domain, domain.map(key => (key, distribution(key) + other.distribution(key))).toMap)
  }

  def -(other: Distribution[A]): Distribution[A] = {
    require(domain == other.domain, "subtraction for distributions requires the same domains")

    Distribution(domain, domain.map(key => (key, distribution(key) - other.distribution(key))).toMap)
  }

  /**
   * Inversely scales the distribution according to a scalar: pr(domain) * 1 / scalar = pr(domain) / scalar
   *
   * This may de-normalize the distribution.
   *
   * @param scalar A scalar bigger than 0.
   * @return The scaled distribution.
   */
  @throws[IllegalArgumentException]
  def /(scalar: BigNatural): Distribution[A] = {
    require(scalar != 0, "Cannot divide by 0.")
    Distribution(domain, distribution.mapValues(_ / scalar))
  }

  /** Efficient representation for sampling. Compute when needed and only once. */
  lazy private val pTree = ProbabilityTree(this)

  /**
   * Draws a sample from the distribution, proportionate to the probabilities.
   *
   * @return A sample
   */
  def sample: A = pTree(Random.nextDouble() * sum)

  /**
   * Returns an iterator containing {{{n}}} samples.
   *
   * @param n
   * @return
   */
  def sample(n: Int): Iterator[A] = new Iterator[A]() {
    private var sampleCount = 0

    override def hasNext: Boolean = sampleCount < n

    override def next(): A = {
      sampleCount += 1
      sample
    }
  }

  def isNormalized: Boolean = sum == 1

  def accuracy: BigNatural = 1.0.toBigNatural - sum

  /** Returns the value in the domain with the maximum probability.
   * If multiple maxima exist, it returns one of those at random.
   *
   * @return Most probable value in the domain
   */
  @throws[IndexOutOfBoundsException]
  def argMax: A = {
    val maxValue = distribution.values.max
    val maxValSet = distribution.filter(_._2 == maxValue).keys.toList
    // If one or more maxima exist return random
    maxValSet(Random.nextInt(maxValSet.length))
  }

  def exp: Distribution[A] = Distribution(domain, distribution.mapValues(_.exp))

  def log: Distribution[A] = Distribution(domain, distribution.mapValues(_.log))

  def softmax(beta: BigNatural): Distribution[A] = softmax(beta, domain.nilDistribution)

  /**
   * Returns the softmaxed distribution
   *
   * @param beta The beta parameter
   * @param costs The cost function as distribution (doesn't have to be normalized)
   * @return A value in the domain of distribution
   * @see See this Wikipedia page for a mathmatical definition of soft argmax
   *      [[https://en.wikipedia.org/wiki/Softmax_function]].
   */
  def softmax(beta: BigNatural, costs: Distribution[A]): Distribution[A] = {
    val softened = (this.log * beta - costs).exp
    val norm = softened.distribution.values.fold(0.toBigNatural)(_ + _)

    val newDistr = (for(v <- domain) yield {
      (v) -> softened.pr(v) / norm
    }).toMap

    Distribution(domain, newDistr)
  }

  /** Returns the Shannon information entropy of this distribution.
   *
   * For distributions that deviate from probability assumptions (i.e., the sum of the values
   * equals 1.0), Shannon information entropy is ill-defined.
   *
   * @return Entropy of the distribution
   */
  def entropy: BigNatural = {
    distribution.values.foldLeft(BigNatural(0))((e: BigNatural, p: BigNatural) =>
      e - (if(p > 0) p * BigNatural.log(p) / BigNatural.log(2) else BigNatural(0))
    )
  }

  /**
   * Returns the Kullback-Leibler divergence from that to this.
   * @param that Other distribution.
   * @return
   */
  def klDivergence(that: Distribution[A]): BigNatural = {
    require(domain == that.domain, "KL-divergence requires equivalent domains.")

    domain.foldLeft(BigNatural(0.0))((kl: BigNatural, value: A) =>
      if(that.pr(value) == 0) kl
      else kl + pr(value) * (pr(value) / that.pr(value)).log
    )
  }

  /**
   * @return The sum of the probabilities, i.e., the probability mass.
   */
  def sum: BigNatural = distribution.values.fold(BigNatural(0))((acc: BigNatural, p: BigNatural) => acc + p)

  override def toString: String = distribution.mkString("{", ", ", "}")

  /** Prints the distribution in a histogram. */
  def hist(): Unit = {
    val maxStrLen = domain.map(_.toString.length).max

    domain.foreach(value => {
      val p = distribution(value)
      val hs = List.tabulate((20 * p).intValue())(_ => "#").mkString
      println(
        value.toString +
          " " * (maxStrLen - value.toString.length + 1) +
          f"${p.doubleValue()}%1.4f\t$hs")
    }
    )
  }
}

/** Factory for [[Distribution]] instances. */
case object Distribution {
  def apply[A, X: ClassTag](domain: Vector[A], distribution: Vector[Double]): Distribution[A] =
    Distribution(domain.toSet, (domain zip distribution.map(BigNatural(_))).toMap)

  def apply[A, X: ClassTag, Y: ClassTag](domain: Vector[A], distribution: Vector[BigNatural]): Distribution[A] =
    Distribution(domain.toSet, (domain zip distribution).toMap)

  def apply[A, X: ClassTag, Y: ClassTag](domain: Set[A], distribution: Map[A, Double]): Distribution[A] =
    Distribution(domain, distribution.mapValues(BigNatural(_)))
}