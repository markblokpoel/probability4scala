package com.markblokpoel.probability4scala

object Implicits {

  implicit class ImplDistributionConstruct[A](domain: Set[A]) {
    def uniformDistribution: Distribution[A] =
      Distribution(domain, domain.map(value => value -> BigDecimal(1.0) / BigDecimal(domain.size)).toMap)

    def singleValueDistribution(value: A): Distribution[A] = {
      Distribution(domain, domain.map(v => v -> (if(v == value) 1.0 else 0.0)).toMap)
    }
  }

  implicit class ImplConditional[A](value: A) {
    def |[B](condition: B): Conditional1[A, B] = Conditional1(value, condition)

    def |[B](condition: Distribution[B]): Conditional2[A, B] = Conditional2(value, condition)
  }

  implicit class ImplDouble(double: Double) {
    def toBigDecimal: BigDecimal = BigDecimal(double)
  }

  implicit class ImplInt(int: Int) {
    def toBigDecimal: BigDecimal = BigDecimal(int)
  }

}
