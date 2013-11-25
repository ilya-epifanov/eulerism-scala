package ru.smartislav.poly

import scala.annotation.tailrec

case class PolynomialBasis(dims: Polynomial*) {
  def minimize(): PolynomialBasis = {
    val minimized = dims.filter(p => dims.filter(r => p != r).forall(r => !p.isReducible(r)))
    if (dims == minimized)
      this
    else
      PolynomialBasis(minimized: _*)
  }

  def reduceStep(): PolynomialBasis = {
    val reduced = dims.map { d =>
      dims.find { r =>
        val rlpp = r.lpp
        if (r eq d) {
          false
        } else {
          //          println(s"Checking $d against $rlpp")
          d.monomials.exists(m => m.isDivisibleBy(rlpp))
        }
      } map {
        p =>
          d reduce p
      } getOrElse d
    }
    if (dims == reduced)
      this
    else
      PolynomialBasis(reduced: _*)
  }

  def normalize(): PolynomialBasis = {
    PolynomialBasis(dims.map(_.normalize()): _*)
  }

  @tailrec
  final def reduce(): PolynomialBasis = {
    val reduced = minimize().reduceStep()
    if (reduced eq this)
      this
    else
      reduced.reduce()
  }

  def groebner(): PolynomialBasis = gröbner()

  def gröbner(): PolynomialBasis = buchbergers()

  def buchbergers(): PolynomialBasis = {
    dims.toList match {
      case h :: t => PolynomialBasis(PolynomialBasis.build(List(h), t): _*)
      case Nil => PolynomialBasis()
    }
  }
}

object PolynomialBasis {
  @tailrec
  private[poly] def build(checked: List[Polynomial], left: List[Polynomial]): Seq[Polynomial] = {
    left match {
      case l :: ls =>
        build(checked ++ List(l), ls ++ checkOne(l, checked, left))
      case Nil => checked
    }
  }

  def checkOne(f: Polynomial, checked: List[Polynomial], left: List[Polynomial]): List[Polynomial] = {
    if (checked.isEmpty) {
      Nil
    } else {
      if (f.lpp coprimeUpToCoeff checked.head.lpp) // Product Criterion
        return checkOne(f, checked.tail, left)

      val s = (f sPoly checked.head) reduceByBasis (checked ++ left)
      if (s.nonZero)
        s :: checkOne(f, checked.tail, s :: left)
      else
        checkOne(f, checked.tail, left)
    }
  }
}
