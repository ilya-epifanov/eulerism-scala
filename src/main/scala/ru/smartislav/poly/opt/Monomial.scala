package ru.smartislav.poly.opt

import spire.math.Rational

case class Monomial(c: Rational, pp: PowerProduct) {
  def *(f: Rational): Monomial = Monomial(c * f, pp)

  def /(f: Rational): Monomial = Monomial(c / f, pp)

  def unary_-(): Monomial = Monomial(-c, pp)

  def isSimilarTo(m: Monomial): Boolean = pp == m.pp

  def +(m: Monomial): Monomial = {
    assert(isSimilarTo(m))
    Monomial(c + m.c, pp)
  }

  def -(m: Monomial): Monomial = {
    assert(isSimilarTo(m))
    Monomial(c - m.c, pp)
  }

  def *(pp: PowerProduct): Monomial = {
    Monomial(c, this.pp * pp)
  }

  def *(m: Monomial): Monomial = {
    Monomial(c * m.c, pp * m.pp)
  }

  def isDivisibleBy(m: Monomial): Boolean = {
    m.c != Rational.zero && pp.isDivisibleBy(m.pp)
  }

  def /(m: Monomial): Monomial = {
    assert(isDivisibleBy(m))
    Monomial(c / m.c, pp / m.pp)
  }

  def /(pp: PowerProduct): Monomial = {
    Monomial(c, this.pp / pp)
  }
}

object Monomial {
  val zero = Monomial(Rational.zero, new PowerProduct(null))
  val one = Monomial(Rational.one, new PowerProduct(null))

  private abstract class MonomialOrderingUpToCoeff(val ord: Ordering[PowerProduct]) extends Ordering[Monomial] {
    def compare(x: Monomial, y: Monomial): Int = ord.compare(x.pp, y.pp)
  }

  object AscPurelexOrdering extends MonomialOrderingUpToCoeff(PowerProduct.AscPurelexOrdering)

  implicit object PurelexOrdering extends MonomialOrderingUpToCoeff(PowerProduct.PurelexOrdering)

}
