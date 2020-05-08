package com.markblokpoel.probability4scala

object Demo extends App {

  import com.markblokpoel.probability4scala.Implicits._
  import com.markblokpoel.probability4scala.DistributionHelpers._

  case class Ref(label: String)

  val signals = Set("monkey", "pizza", "tree", "couch") // domain
  val signalPriors = signals.uniformDistribution // create uniform distribution
  signalPriors.hist() // print histogram
  val p1 = pr("monkey", signalPriors) // request probability
  println(p1)

  // create custom distribution
  val (r1, r2, r3) = (Ref("r1"), Ref("r2"), Ref("r3"))
  val referents = Vector(r1, r2, r3)
  val customDistribution = Vector(.1, .4, .5)
  val referentPriors = Distribution(referents, customDistribution)
  referentPriors.hist()

  println(referentPriors.sample(10).mkString("\n"))

  val p2 = pr(Ref("r2"), referentPriors)
  println(s"pr(r2)=$p2")

  // conditional distribution
  val cd = Map(
    ("monkey", r1) -> .2,
    ("pizza", r1) -> .4,
    ("tree", r1) -> .0,
    ("couch", r1) -> .4,
    ("monkey", r2) -> .5,
    ("pizza", r2) -> .1,
    ("tree", r2) -> .2,
    ("couch", r2) -> .2,
    ("monkey", r3) -> .1,
    ("pizza", r3) -> .2,
    ("tree", r3) -> .4,
    ("couch", r3) -> .3
  )

  val cdistr = ConditionalDistribution(signals, referents.toSet, cd)

  cdistr.cpt()

  val p3 = prV1("monkey", cdistr) // this will marginalize over all possible conditional val
  println(s"pr(monkey) = sum_r pr(monkey|r) = $p3")
  val posterior = cdistr * referentPriors // this will conditionalize over referent priors
  posterior.hist()

//  cdistr.bayes(referents.uniformDistribution).table()

  // RSA test

  val (s1, s2, s3) = ("s1", "s2", "s3")
  val (rr1, rr2, rr3) = ("r1", "r2", "r3")

  val rsaSignals = Set(s1, s2, s3)
  val rsaReferents = Set(rr1, rr2)//, rr3)

  val li0f = Map(
    (rr1, s1) -> 1.0,
    (rr2, s1) -> 1.0,
//    (rr3, s1) -> 1.0,
    (rr1, s2) -> 0.0,
    (rr2, s2) -> 1.0,
//    (rr3, s2) -> 1.0,
    (rr1, s3) -> 1.0,
    (rr2, s3) -> 0.0,
//    (rr3, s3) -> 1.0,
  )

  def lex2distrF[A, B](lex: Map[(A, B), Double]): Map[(A, B), BigDecimal] = {
    val sum = lex.values.sum
    val domain1 = lex.keySet.map(_._1).toVector
    val domain2 = lex.keySet.map(_._2).toVector
    domain1.flatMap(d1 => {
      domain2.map(d2 => {
        (d1, d2) -> lex((d1, d2)).toBigDecimal / sum
      })
    }).toMap
  }

  println("===L0===")
  val li0 =
    ConditionalDistribution(rsaReferents, rsaSignals, lex2distrF(li0f))

  li0.cpt()

  println("")
  pr("s1", li0).hist()
  pr("s2", li0).hist()
  pr("s3", li0).hist()

  println("")

  println(li0.prV2("s1"))

  println("===S1===")
  val sp1 =
    li0.bayes(rsaSignals.uniformDistribution)
  sp1.cpt()
  println(sp1.sum)
  println("")
  pr("r1", sp1).hist()
  pr("r2", sp1).hist()



  println("===L1===")
  val li1 =
    sp1.bayes(rsaReferents.uniformDistribution)
  li1.cpt()
  println(li1.sum)

  println("")
  pr("s1", li1).hist()
  pr("s2", li1).hist()
  pr("s3", li1).hist()


  println("s1" | rsaReferents.uniformDistribution)


}