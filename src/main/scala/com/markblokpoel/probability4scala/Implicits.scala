package com.markblokpoel.probability4scala

import com.markblokpoel.probability4scala.datastructures.BigNatural

/** Implicit functions for more concise syntax. */
object Implicits {

  implicit class ImplDistributionConstruct[A](domain: Set[A]) {
    /**
     * Constructs a uniform distribution over the domain.
     * @return
     */
    def uniformDistribution: Distribution[A] =
      Distribution(domain, domain.map(value => value -> BigDecimal(1.0) / BigDecimal(domain.size)).toMap)

    /**
     * Constructs a distribution over the domain where pr(a) = 1.0 if v == value and 0.0 otherwise.
     * @param value value in the domain to construct distribution for
     * @return
     */
    @throws[IllegalArgumentException]
    def singleValueDistribution(value: A): Distribution[A] = {
      require(domain.contains(value), "Cannot construct distribution for value outside the domain.")
      Distribution(domain, domain.map(v => v -> (if(v == value) 1.0 else 0.0)).toMap)
    }
  }

  implicit class ImplConditional[A](value: A) {
    /**
     * Syntax for writing a conditional given a value.
     * @param condition condition value
     * @tparam B type of the condition domain
     * @return
     */
    def |[B](condition: B): Conditional1[A, B] = Conditional1(value, condition)

    /**
     * Syntax for writing a conditional given a distribution.
     * @param condition condition value
     * @tparam B type of the condition domain
     * @return
     */
    def |[B](condition: Distribution[B]): Conditional2[A, B] = Conditional2(value, condition)
  }

  implicit class ImplDouble(double: Double) {
    /** Converts Double to BigDecimal. */
    def toBigDecimal: BigDecimal = BigDecimal(double)

    /** Converts Double to BigDecimalInf. */
    def toBigNatural: BigNatural = BigNatural(double, double.isPosInfinity, double.isNegInfinity)

    def %(nat: BigNatural): BigNatural = BigNatural(double) % nat
    def *(nat: BigNatural): BigNatural = BigNatural(double) * nat
    def +(nat: BigNatural): BigNatural = BigNatural(double) + nat
    def -(nat: BigNatural): BigNatural = BigNatural(double) - nat
    def /(nat: BigNatural): BigNatural = BigNatural(double) / nat
    def /%(nat: BigNatural): (BigNatural, BigNatural) = BigNatural(double) /% nat
    def <(nat: BigNatural): Boolean = BigNatural(double) < nat
    def <=(nat: BigNatural): Boolean = BigNatural(double) <= nat
    def >(nat: BigNatural): Boolean = BigNatural(double) > nat
    def >=(nat: BigNatural): Boolean = BigNatural(double) >= nat
    def min(nat: BigNatural): BigNatural = BigNatural(double) min nat
    def max(nat: BigNatural): BigNatural = BigNatural(double) max nat
    def quot(nat: BigNatural): BigNatural = BigNatural(double) quot nat
    def remainder(nat: BigNatural): BigNatural = BigNatural(double) remainder nat
  }

  implicit class ImplInt(int: Int) {
    /** Converts Int to BigDecimal. */
    def toBigDecimal: BigDecimal = BigDecimal(int)

    /** Converts Int to BigDecimalInf. */
    def toBigNatural: BigNatural = BigNatural(int)
  }

  implicit class ImplBigNatural(nat: BigNatural) {
    def log: BigNatural = nat.log
    def exp: BigNatural = nat.exp
  }

}
