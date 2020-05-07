package com.markblokpoel.probability4scala

case class Conditional[A, B](value: A, condition: B) {
  override def toString: String = s"$value | $condition"
}
