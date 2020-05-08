package com.markblokpoel.probability4scala.datastructures

class PBranch[A](val left: PTree[A], val right: PTree[A], val threshold: BigDecimal) extends PTree[A] {
  def apply(t: BigDecimal): A =
    if (t <= threshold) left(t)
    else right(t)

  override def toString: String =
    "<=" + threshold + "(" + left + ", " + right + ")"
}

object PBranch {
  def apply[A](left: PTree[A], right: PTree[A], threshold: BigDecimal): PBranch[A] =
    new PBranch(left, right, threshold)
}