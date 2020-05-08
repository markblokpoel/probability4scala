package com.markblokpoel.probability4scala.datastructures

import com.markblokpoel.probability4scala.Distribution

case class ProbabilityTree[A](distribution: Distribution[A]) {
  private val values = distribution.distribution.keys.toVector
  private val probabilities = values.map(distribution.distribution(_))
  val tree: PTree[A] = constructPTree(values, probabilities, 0)

  def apply(p: BigDecimal): A = tree(p)

  private def constructPTree(values: Vector[A], probabilities: Vector[BigDecimal], p: BigDecimal): PTree[A] = {
    val (leftValues, rightValues) = values.splitAt(values.length/2)
    val (leftProbabilities, rigthProbabilities) = probabilities.splitAt(probabilities.length/2)
    val leftSum = leftProbabilities.sum

    if(values.tail.isEmpty) PLeaf(values.head)
    else {
      PBranch(constructPTree(leftValues, leftProbabilities, 0),
        constructPTree(rightValues, rigthProbabilities, leftSum + p),
        leftSum + p)
    }
  }
}
