package com.markblokpoel.probability4scala

import com.markblokpoel.probability4scala.Demo.sd
import com.markblokpoel.probability4scala.DistributionHelpers.{exp, log}
import com.markblokpoel.probability4scala.datastructures.ProbabilityTree
import com.markblokpoel.probability4scala.Implicits._

import scala.reflect.ClassTag
import scala.util.Random

/**
 * Implements a generic probability distribution. The probabilities are represented by BigDecimal for increased
 * accuracy.
 *
 * @param domain       The set of values that span this distribution.
 * @param distribution The probabilities for each value in the domain.
 * @tparam A The type of the domain.
 */
case class Distribution[A](domain: Set[A], distribution: Map[A, BigDecimal]) {

  /**
   * @param value A value within the domain.
   * @return The probability of the value.
   */
  @throws[NoSuchElementException]
  def pr(value: A): BigDecimal = distribution(value)

  /**
   * Scales the distribution according to the scalar: pr(domain) * scalar
   *
   * This may de-normalize the distribution.
   *
   * @param scalar
   * @return The scaled distribution.
   */
  def *(scalar: BigDecimal): Distribution[A] = Distribution(domain, distribution.mapValues(_ * scalar))

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
  def /(scalar: BigDecimal): Distribution[A] = {
    require(scalar > 0, "Cannot divide by 0.")
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

  def accuracy: BigDecimal = 1.0.toBigDecimal - sum

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

  def exp: Distribution[A] = Distribution(domain, distribution.mapValues(bd => math.exp(bd.doubleValue).toBigDecimal))

  def log: Distribution[A] = Distribution(domain, distribution.mapValues(bd => math.log(bd.doubleValue).toBigDecimal))

  /**
   * Returns the softmaxed distribution
   *
   * @param beta The beta parameter
   * @return A value in the domain of distribution
   * @see See this Wikipedia page for a mathmatical definition of soft argmax
   *      [[https://en.wikipedia.org/wiki/Softmax_function]].
   */
  def softArgMax(beta: Double): Distribution[A] = (this.log * beta).exp / (this.log * beta).exp.sum

  /** Returns the Shannon information entropy of this distribution.
   *
   * For distributions that deviate from probability assumptions (i.e., the sum of the values
   * equals 1.0), Shannon information entropy is ill-defined.
   *
   * @return Entropy of the distribution
   */
  def entropy: Double = {
    distribution.values.foldLeft(0.0)((e: Double, p: BigDecimal) =>
        e - (if (p > 0) p.doubleValue * math.log(p.doubleValue) / math.log(2) else 0)
    )
  }

  /**
   * @return The sum of the probabilities, i.e., the probability mass.
   */
  def sum: BigDecimal = distribution.values.sum

  override def toString: String = distribution.mkString("{", ", ", "}")

  /** Prints the distribution in a histogram. */
  def hist(): Unit = {
    val maxStrLen = domain.map(_.toString.length).max

    domain.foreach(value => {
      val p = distribution(value)
      val hs = List.tabulate((20 * p).intValue)(_ => "#").mkString
      println(
        value.toString +
          " " * (maxStrLen - value.toString.length + 1) +
          f"$p%1.4f\t$hs")
    }
    )
  }
}

/** Factory for [[Distribution]] instances. */
case object Distribution {
  def apply[A, X: ClassTag](domain: Vector[A], distribution: Vector[Double]): Distribution[A] =
    Distribution(domain.toSet, (domain zip distribution.map(BigDecimal(_))).toMap)

  def apply[A, X: ClassTag, Y: ClassTag](domain: Vector[A], distribution: Vector[BigDecimal]): Distribution[A] =
    Distribution(domain.toSet, (domain zip distribution).toMap)

  def apply[A, X: ClassTag, Y: ClassTag](domain: Set[A], distribution: Map[A, Double]): Distribution[A] =
    Distribution(domain, distribution.mapValues(BigDecimal(_)))
}