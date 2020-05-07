package com.markblokpoel.probability4scala

import scala.reflect.{ClassManifest, ClassTag}

case class Distribution[A](domain: Set[A], distribution:  Map[A, BigDecimal]) {

  def pr(value: A): BigDecimal = distribution(value)

  def *(scalar: BigDecimal): Distribution[A] = Distribution(domain, distribution.view.mapValues(_ * scalar).toMap)

  def /(scalar: BigDecimal): Distribution[A] = {
    require(scalar > 0, "Distribution./ distribution divided by zero scalar")
    Distribution(domain, distribution.view.mapValues(_ / scalar).toMap)
  }

  def sum: BigDecimal = distribution.values.sum

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