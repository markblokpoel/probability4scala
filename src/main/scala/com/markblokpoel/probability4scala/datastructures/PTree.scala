package com.markblokpoel.probability4scala.datastructures

trait PTree[A] {
  def apply(t: BigNatural): A
}
