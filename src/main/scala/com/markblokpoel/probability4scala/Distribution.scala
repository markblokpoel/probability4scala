package com.markblokpoel.probability4scala

import scala.reflect.ClassTag
import scala.util.Random

case class Distribution[A](domain: Set[A], distribution:  Map[A, BigDecimal]) {

  def pr(value: A): BigDecimal = distribution(value)

  def *(scalar: BigDecimal): Distribution[A] = Distribution(domain, distribution.view.mapValues(_ * scalar).toMap)

  def /(scalar: BigDecimal): Distribution[A] = {
    require(scalar > 0, "Distribution./ distribution divided by zero scalar")
    Distribution(domain, distribution.view.mapValues(_ / scalar).toMap)
  }

  /** Draws a sample from the distribution, proportionate to the probabilities.
   *
   * @return
   */
//  def sample: A = {
//    val pt = Random.nextDouble()
//
//    @scala.annotation.tailrec
//    def sample(acc: BigDecimal,
//               domain: Iterable[A]): A = {
//      if (acc <= pt && pt < acc + distribution.head) domain.head
//      else sample(acc + distribution.head, domain.tail, distribution.tail)
//    }
//
//    sample(0.0, distribution.keys)
//  }

  def sum: BigDecimal = distribution.values.sum

  override def toString: String = distribution.mkString("{", ", ", "}")

  def hist(): Unit = {
    val maxStrLen = domain.map(_.toString.length).max

    domain.foreach(value => {
      val p = distribution(value)
      val hs = List.tabulate((20 * p).intValue)(_ => "#").mkString
      println(
        value.toString +
          " " * (maxStrLen - value.toString.length + 1) +
        f"$p%2.4f\t$hs")
    }
    )
  }
}

case object Distribution {
  def apply[A, X: ClassTag](domain: Vector[A], distribution: Vector[Double]): Distribution[A] =
    Distribution(domain.toSet, (domain zip distribution.map(BigDecimal(_))).toMap)

  def apply[A, X: ClassTag, Y: ClassTag](domain: Vector[A], distribution: Vector[BigDecimal]): Distribution[A] =
    Distribution(domain.toSet, (domain zip distribution).toMap)

  def apply[A, X: ClassTag, Y: ClassTag](domain: Set[A], distribution: Map[A, Double]): Distribution[A] =
    Distribution(domain, distribution.view.mapValues(BigDecimal(_)).toMap)
}