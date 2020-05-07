package com.markblokpoel.probability4scala

import Implicits._

import scala.reflect.ClassTag

case class ConditionalDistribution[A, B](domain1: Set[A], domain2: Set[B], distribution: Map[(A, B), BigDecimal]) {
  def pr(conditional: Conditional[A, B]): BigDecimal = distribution((conditional.value, conditional.condition))

  def pr(given: B): Distribution[A] = this * domain2.singleValueDistribution(given) / prB(given)

  // marginalization
  def prA(value: A): BigDecimal = domain2.foldLeft(0.toBigDecimal) {
    (acc: BigDecimal, conditional: B) => acc + pr(value | conditional)
  }

  def prB(conditional: B): BigDecimal = domain1.foldLeft(0.toBigDecimal) {
    (acc: BigDecimal, value: A) => acc + pr(value | conditional)
  }

  // marginal distribution
  def marginalDistributionA: Distribution[A] =
    Distribution(domain1, domain1.map(value => value -> prA(value)).toMap)

  def marginalDistributionB: Distribution[B] =
    Distribution(domain2, domain2.map(value => value -> prB(value)).toMap)

  // posterior distribution
  def *(prior: Distribution[B]): Distribution[A] = Distribution(domain1,
    domain1.map((value1: A) =>
      value1 -> domain2.map((value2: B) => pr(value1 | value2) * prior.pr(value2)).sum
    ).toMap
  )

  // conditionalization
  def pr(value: A, prior: Distribution[B]): BigDecimal =
    domain2.map(condition => pr(value | condition) * prior.pr(condition)).sum

  // Bayes' rule
  def bayes(domain2Prior: Distribution[B], domain1Prior: Distribution[A]): ConditionalDistribution[B, A] = {
    val newDistribution: Set[((B, A), BigDecimal)] = domain2.flatMap(condition => {
      domain1.map(value => {
        (condition, value) -> pr(value | condition) * domain2Prior.pr(condition) / domain1Prior.pr(value)
      })
    })
    ConditionalDistribution(domain2, domain1, newDistribution.toMap)
  }

  def sum: BigDecimal = distribution.values.sum

  def table(): Unit = {
    val maxStrLen1 = domain1.map(_.toString.length).max
    val maxStrLen2 = math.max(6, domain2.map(_.toString.length).max)
    println(" " * (maxStrLen1+1) + domain2.map(d2 => d2.toString + " " * (maxStrLen2 - d2.toString.length + 1)).mkString)
    domain1.foreach { d1 =>
      println(
        d1.toString + " " * (maxStrLen1 - d1.toString.length + 1) +
          domain2.toVector.map(d2 => f"${pr(d1 | d2)}%2.4f" + " " * (maxStrLen2 - f"${pr(d1 | d2)}%2.4f".length + 1)).mkString
      )
    }
  }
}

case object ConditionalDistribution {
  def apply[A, B, X: ClassTag](domain1: Set[A], domain2: Set[B], distribution: Map[(A, B), Double]): ConditionalDistribution[A, B] = {
    val bdd = distribution.view.mapValues(BigDecimal(_)).toMap
    ConditionalDistribution[A, B](domain1, domain2, bdd)
  }
}

