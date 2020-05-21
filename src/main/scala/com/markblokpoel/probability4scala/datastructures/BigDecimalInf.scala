package com.markblokpoel.probability4scala.datastructures

import java.math.MathContext

import scala.collection.immutable.NumericRange.{Exclusive, Inclusive}
import scala.collection.immutable.Range.Partial
import scala.math.BigDecimal.RoundingMode.RoundingMode
import spire.std.BigDecimalIsTrig

class BigDecimalInf(val dec: BigDecimal, val isPositiveInfinite: Boolean = false, val isNegativeInfinite: Boolean = false) {

  val bdt = new BigDecimalIsTrig

  def this(d: Double) {
    this(
      if (d.isInfinite) BigDecimal(0) else BigDecimal(d),
      d.isPosInfinity,
      d.isNegInfinity
    )
  }

  def this(i: Int) {
    this(i, false, false)
  }

  def isInfinite: Boolean = isPositiveInfinite || isNegativeInfinite

  def exp: BigDecimalInf =
    if(isNegativeInfinite) new BigDecimalInf(0, false, false)
    else if(isPositiveInfinite) this
    else new BigDecimalInf(bdt.exp(dec))

  def log: BigDecimalInf =
    if(dec == 0) new BigDecimalInf(0, false, true)
    else new BigDecimalInf(bdt.log(dec), false, false)

//  def pow(that: BigDecimalInf): BigDecimalInf = bdt.

  def %(that: BigDecimalInf): BigDecimalInf =
    if(isInfinite) this
    else new BigDecimalInf(dec % that.dec, false, false)

  def *(that: BigDecimalInf): BigDecimalInf =
    if(isInfinite) this
    else new BigDecimalInf(dec * that.dec, false, false)

  def +(that: BigDecimalInf): BigDecimalInf =
    if(isInfinite) this
    else new BigDecimalInf(dec + that.dec, false, false)

  def -(that: BigDecimalInf): BigDecimalInf =
    if(isInfinite) this
    else new BigDecimalInf(dec - that.dec, false, false)

  def /(that: BigDecimalInf): BigDecimalInf =
    if(isInfinite) this
    else new BigDecimalInf(dec / that.dec, false, false)

  def /%(that: BigDecimalInf): (BigDecimalInf, BigDecimalInf) =
    if(isInfinite) (this, this)
    else {
      val (div, rest) = dec /% that.dec
      (new BigDecimalInf(div, false, false), new BigDecimalInf(rest, false, false))
    }

  def <(that: BigDecimalInf): Boolean =
    if(isPositiveInfinite && that.isPositiveInfinite) false
    else if(isNegativeInfinite && that.isNegativeInfinite) false
    else if(isPositiveInfinite) false
    else if(isNegativeInfinite) true
    else dec < that.dec

  def <=(that: BigDecimalInf): Boolean =
    if(isPositiveInfinite && that.isPositiveInfinite) true
    else if(isNegativeInfinite && that.isNegativeInfinite) true
    else if(isPositiveInfinite) false
    else if(isNegativeInfinite) true
    else dec <= that.dec

  def >(that: BigDecimalInf): Boolean =
    if(isPositiveInfinite && that.isPositiveInfinite) false
    else if(isNegativeInfinite && that.isNegativeInfinite) false
    else if(isPositiveInfinite) true
    else if(isNegativeInfinite) false
    else dec > that.dec

  def >=(that: BigDecimalInf): Boolean =
    if(isPositiveInfinite && that.isPositiveInfinite) true
    else if(isNegativeInfinite && that.isNegativeInfinite) true
    else if(isPositiveInfinite) true
    else if(isNegativeInfinite) false
    else dec >= that.dec

  def abs: BigDecimalInf = new BigDecimalInf(dec.abs, isPositiveInfinite, isNegativeInfinite)

  def apply(mc: MathContext): BigDecimalInf = new BigDecimalInf(dec(mc), isPositiveInfinite, isNegativeInfinite)

  def byteValue(): Byte =
    if(isPositiveInfinite) (-1).byteValue()
    else if(isNegativeInfinite) 0.byteValue()
    else dec.byteValue()

  def charValue: Char =
    if(isPositiveInfinite) Double.PositiveInfinity.toChar
    else if(isNegativeInfinite) Double.NegativeInfinity.toChar
    else dec.charValue

  def compare(that: BigDecimalInf): Int =
    if(isPositiveInfinite && that.isPositiveInfinite) 0
    else if(isNegativeInfinite && that.isNegativeInfinite) 0
    else if(isPositiveInfinite) 1
    else if(isNegativeInfinite) -1
    else dec.compare(that.dec)

  def doubleValue(): Double =
    if(isPositiveInfinite) Double.PositiveInfinity
    else if(isNegativeInfinite) Double.NegativeInfinity
    else dec.doubleValue()

  def equals(that: BigDecimalInf): Boolean =
    if(isPositiveInfinite && that.isPositiveInfinite) true
    else if(isNegativeInfinite && that.isNegativeInfinite) true
    else if(isPositiveInfinite) false
    else if(isNegativeInfinite) false
    else dec.equals(that.dec)

  override def equals(that: Any): Boolean = this.equals(that)

  def floatValue(): Float =
    if(isPositiveInfinite) Float.PositiveInfinity
    else if(isNegativeInfinite) Float.NegativeInfinity
    else dec.floatValue()

  override def hashCode(): Int =
    if(isPositiveInfinite) Double.PositiveInfinity.hashCode()
    else if(isNegativeInfinite) Double.NegativeInfinity.hashCode()
    else dec.hashCode()

  def intValue(): Int =
    if(isPositiveInfinite) Int.MaxValue
    else if(isNegativeInfinite) Int.MinValue
    else dec.intValue()

  def isValidByte: Boolean =
    if(isPositiveInfinite) Double.PositiveInfinity.isValidByte
    else if(isNegativeInfinite) Double.NegativeInfinity.isValidByte
    else dec.isValidByte

  def isValidChar: Boolean =
    if(isPositiveInfinite) Double.PositiveInfinity.isValidChar
    else if(isNegativeInfinite) Double.NegativeInfinity.isValidChar
    else dec.isValidChar

  def isValidInt: Boolean =
    if(isPositiveInfinite) Double.PositiveInfinity.isValidInt
    else if(isNegativeInfinite) Double.NegativeInfinity.isValidInt
    else dec.isValidInt

  def isValidShort: Boolean =
    if(isPositiveInfinite) Double.PositiveInfinity.isValidShort
    else if(isNegativeInfinite) Double.NegativeInfinity.isValidShort
    else dec.isValidShort

  def longValue(): Long =
    if(isPositiveInfinite) Long.MaxValue
    else if(isNegativeInfinite) Long.MinValue
    else dec.longValue()

  def max(that: BigDecimalInf): BigDecimalInf =
    if(isPositiveInfinite) this
    else if(isNegativeInfinite) that
    else new BigDecimalInf(dec.max(that.dec), false, false)

  val mc: MathContext = dec.mc

  def min(that: BigDecimalInf): BigDecimalInf =
    if(isPositiveInfinite) that
    else if(isNegativeInfinite) this
    else new BigDecimalInf(dec.min(that.dec), false, false)

  def pow(n: Int): BigDecimalInf = new BigDecimalInf(dec.pow(n), isPositiveInfinite, isNegativeInfinite)

  def precision: Int = dec.precision

  def quot(that: BigDecimalInf): BigDecimalInf =
    if(isInfinite) this
    else new BigDecimalInf(dec.quot(that.dec), false, false)

  def remainder(that: BigDecimalInf): BigDecimalInf =
    if(isInfinite) this
    else new BigDecimalInf(dec.remainder(that.dec), false, false)

  def round(mc: MathContext): BigDecimalInf =
    if(isInfinite) this
    else new BigDecimalInf(dec.round(mc), false, false)

  def scale: Int = dec.scale

  def setScale(scale: Int, mode: RoundingMode): BigDecimalInf =
    new BigDecimalInf(dec.setScale(scale, mode), isPositiveInfinite, isNegativeInfinite)

  def setScale(scale: Int): BigDecimalInf =
    new BigDecimalInf(dec.setScale(scale), isPositiveInfinite, isNegativeInfinite)

  def shortValue(): Short =
    if(isPositiveInfinite) Short.MaxValue
    else if(isNegativeInfinite) Short.MinValue
    else dec.shortValue()

  def signum: Int =
    if(isPositiveInfinite) 1
    else if(isNegativeInfinite) -1
    else dec.signum

  def to(end: BigDecimalInf, step: BigDecimalInf): Inclusive[BigDecimal] = {
    require(!isInfinite && !end.isInfinite && !step.isInfinite, "cannot create range from or to infinity")
    dec.to(end.dec, step.dec)
  }

  def to(end: BigDecimalInf): Partial[BigDecimal, Inclusive[BigDecimal]] = {
    require(!isInfinite && !end.isInfinite, "cannot create range from or to infinity")
    dec.to(end.dec)
  }

  def toBigInt: BigInt = {
    require(!isInfinite, "cannot convert infinite number to BigInt")
    dec.toBigInt()
  }

  def toBigIntExact: Option[BigInt] =
    if(isInfinite) None
    else dec.toBigIntExact()

  def toByte: Byte =
    if(isPositiveInfinite) Double.PositiveInfinity.toByte
    else if(isNegativeInfinite) Double.NegativeInfinity.toByte
    else dec.toByte

  def toByteExact: Byte = {
    require(!isInfinite, "cannot convert infinite number to Byte Exact")
    dec.toByteExact
  }

  def toChar: Char =
    if(isPositiveInfinite) Double.PositiveInfinity.toChar
    else if(isNegativeInfinite) Double.NegativeInfinity.toChar
    else dec.toChar

  def toDouble: Double =
    if(isPositiveInfinite) Double.PositiveInfinity
    else if(isNegativeInfinite) Double.NegativeInfinity
    else dec.toDouble

  def toFloat: Float =
    if(isPositiveInfinite) Float.PositiveInfinity
    else if(isNegativeInfinite) Float.NegativeInfinity
    else dec.toFloat

  def toInt: Int =
    if(isPositiveInfinite) Int.MaxValue
    else if(isNegativeInfinite) Int.MinValue
    else dec.toInt

  def toIntExact: Int =
    if(isPositiveInfinite) Int.MaxValue
    else if(isNegativeInfinite) Int.MinValue
    else dec.toIntExact

  def toLong: Long =
    if(isPositiveInfinite) Long.MaxValue
    else if(isNegativeInfinite) Long.MinValue
    else dec.toLong

  def toLongExact: Long =
    if(isPositiveInfinite) Long.MaxValue
    else if(isNegativeInfinite) Long.MinValue
    else dec.toLongExact

  def toShort: Short =
    if(isPositiveInfinite) Short.MaxValue
    else if(isNegativeInfinite) Short.MinValue
    else dec.toShort

  def toShortExact: Short =
    if(isPositiveInfinite) Short.MaxValue
    else if(isNegativeInfinite) Short.MinValue
    else dec.toShortExact

  override def toString(): String =
    if(isPositiveInfinite) "Infinity"
    else if(isNegativeInfinite) "-Infinity"
    else dec.toString()

  def ulp: BigDecimalInf = {
    require(!isInfinite, "unit of last in place not defined for infinite values")
    new BigDecimalInf(dec.ulp, false, false)
  }

  def unary_- : BigDecimalInf = new BigDecimalInf(dec.unary_-, !isPositiveInfinite, !isNegativeInfinite)

  def underlying(): BigDecimal = dec.underlying()

  def until(end: BigDecimalInf, step: BigDecimalInf): Exclusive[BigDecimal] = {
    require(!isInfinite && !end.isInfinite && !step.isInfinite, "cannot create range from or to infinity")
    dec.until(end.dec, step.dec)
  }

  def until(end: BigDecimalInf): Partial[BigDecimal, Exclusive[BigDecimal]] =
    {
      require(!isInfinite && !end.isInfinite, "cannot create range from or to infinity")
      dec.until(end.dec)
    }

}