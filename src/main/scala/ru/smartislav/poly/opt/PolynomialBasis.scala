package ru.smartislav.poly.opt

import scala.annotation.tailrec

class PolynomialBasis(val dims: Seq[Polynomial]) {
  def minimize(): PolynomialBasis = {
    val minimized = dims.filter(p => dims.filter(r => p != r).forall(r => !p.isReducible(r)))
    if (dims == minimized)
      this
    else
      new PolynomialBasis(minimized)
  }

  def reduceStep(): PolynomialBasis = {
    val reduced = dims.map { d =>
      dims.find { r =>
        val rlpp = r.lpp
        if (r eq d) {
          false
        } else {
          //          println(s"Checking $d against $rlpp")
          d.terms.exists(m => m.pp.isDivisibleBy(rlpp))
        }
      } map {
        p =>
          d reduce p
      } getOrElse d
    }
    if (dims == reduced)
      this
    else
      new PolynomialBasis(reduced)
  }

  def normalize(): PolynomialBasis = {
    new PolynomialBasis(dims.map(_.normalize()))
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
      case h :: t => new PolynomialBasis(PolynomialBasis.build(List(h), t))
      case Nil => new PolynomialBasis(Seq.empty)
    }
  }
}

object PolynomialBasis {
  @tailrec
  private[opt] def build(checked: List[Polynomial], left: List[Polynomial]): Seq[Polynomial] = {
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
      if (f.lpp coprime checked.head.lpp) // Product Criterion
        return checkOne(f, checked.tail, left)

      val s = (f spol checked.head).reduce(new PolynomialBasis(checked ++ left))
      if (s.nonZero)
        s :: checkOne(f, checked.tail, s :: left)
      else
        checkOne(f, checked.tail, left)
    }
  }
}


