package com.markblokpoel.probability4scala.datastructures

import java.math.MathContext

import scala.collection.immutable.NumericRange.{Exclusive, Inclusive}
import scala.collection.immutable.Range.Partial
import scala.math.BigDecimal.RoundingMode.RoundingMode


class BigDecimalInf(val bigDecimal: BigDecimal, val isPositiveInfinite: Boolean = false, val isNegativeInfinite: Boolean = false) {

  def this(d: Double) {
    this(if (d.isInfinite) 0.0 else d, d.isPosInfinity, d.isNegInfinity)
  }

  def this(i: Int) {
    this(if (i.isInfinite) 0.0 else i, i.isPosInfinity, i.isNegInfinity)
  }

  def exp(base: Double = math.E): BigDecimalInf = ???

  def log(base: Double = math.E): BigDecimalInf = ???

  def pow(that: BigDecimalInf): BigDecimalInf = ???

  def %(that: BigDecimal): BigDecimal = bigDecimal % that

  def *(that: BigDecimal): BigDecimal = bigDecimal * that

  def +(that: BigDecimal): BigDecimal = bigDecimal + that

  def -(that: BigDecimal): BigDecimal = bigDecimal - that

  def /(that: BigDecimal): BigDecimal = bigDecimal / that

  def /%(that: BigDecimal): (BigDecimal, BigDecimal) = bigDecimal /% that

  def <(that: BigDecimal): Boolean = bigDecimal < that

  def <=(that: BigDecimal): Boolean = bigDecimal <= that

  def >(that: BigDecimal): Boolean = bigDecimal > that

  def >=(that: BigDecimal): Boolean = bigDecimal >= that

  def abs: BigDecimal = bigDecimal.abs

  def apply(mc: MathContext): BigDecimal = bigDecimal(mc)

  def byteValue(): Byte = bigDecimal.byteValue()

  def charValue: Char = bigDecimal.charValue

  def compare(that: BigDecimal): Int = bigDecimal.compare(that)

  def doubleValue(): Double = bigDecimal.doubleValue()

  def equals(that: BigDecimal): Boolean = bigDecimal.equals(that)

  override def equals(that: Any): Boolean = bigDecimal.equals(that)

  def floatValue(): Float = bigDecimal.floatValue()

  override def hashCode(): Int = bigDecimal.hashCode()

  def intValue(): Int = bigDecimal.intValue()

  def isValidByte: Boolean = bigDecimal.isValidByte

  def isValidChar: Boolean = bigDecimal.isValidChar

  def isValidInt: Boolean = bigDecimal.isValidInt

  def isValidShort: Boolean = bigDecimal.isValidShort

  def longValue(): Long = bigDecimal.longValue()

  def max(that: BigDecimal): BigDecimal = bigDecimal.max(that)

  val mc: MathContext = bigDecimal.mc

  def min(that: BigDecimal): BigDecimal = bigDecimal.min(that)

  def pow(n: Int): BigDecimal = bigDecimal.pow(n)

  def precision: Int = bigDecimal.precision

  def quot(that: BigDecimal): BigDecimal = bigDecimal.quot(that)

  def remainder(that: BigDecimal): BigDecimal = bigDecimal.remainder(that)

  def round(mc: MathContext): BigDecimal = bigDecimal.round(mc)

  def scale: Int = bigDecimal.scale

  def setScale(scale: Int, mode: RoundingMode): BigDecimal = bigDecimal.setScale(scale, mode)

  def setScale(scale: Int): BigDecimal = bigDecimal.setScale(scale)

  def shortValue(): Short = bigDecimal.shortValue()

  def signum: Int = bigDecimal.signum

  def to(end: BigDecimal, step: BigDecimal): Inclusive[BigDecimal] = bigDecimal.to(end, step)

  def to(end: BigDecimal): Partial[BigDecimal, Inclusive[BigDecimal]] = bigDecimal.to(end)

  def toBigInt: BigInt = bigDecimal.toBigInt()

  def toBigIntExact: Option[BigInt] = bigDecimal.toBigIntExact()

  def toByte: Byte = bigDecimal.toByte

  def toByteExact: Byte = bigDecimal.toByteExact

  def toChar: Char = bigDecimal.toChar

  def toDouble: Double = bigDecimal.toDouble

  def toFloat: Float = bigDecimal.toFloat

  def toInt: Int = bigDecimal.toInt

  def toIntExact: Int = bigDecimal.toIntExact

  def toLong: Long = bigDecimal.toLong

  def toLongExact: Long = bigDecimal.toLongExact

  def toShort: Short = bigDecimal.toShort

  def toShortExact: Short = bigDecimal.toShortExact

  override def toString(): String = bigDecimal.toString()

  def ulp: BigDecimal = bigDecimal.ulp

  def unary_- : BigDecimal = bigDecimal.unary_-

  def underlying(): BigDecimal = bigDecimal.underlying()

  def until(end: BigDecimal, step: BigDecimal): Exclusive[BigDecimal] = bigDecimal.until(end, step)

  def until(end: BigDecimal): Partial[BigDecimal, Exclusive[BigDecimal]] = bigDecimal.until(end)

}