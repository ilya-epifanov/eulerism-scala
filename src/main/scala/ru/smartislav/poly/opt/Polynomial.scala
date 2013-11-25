package ru.smartislav.poly.opt

import scalaz.ImmutableArray
import ru.smartislav.eulerism._
import scala.collection._
import spire.math.Rational

case class Polynomial(terms: ImmutableArray[Monomial]) {
  assert(terms.forall(_ != Monomial.zero))

  def lt = terms.head

  def lpp = terms.head.pp

  def +(r: Polynomial): Polynomial = {
    new Polynomial(mergeWith(terms, r.terms)((a: Monomial, b: Monomial) => a + b)
      (implicitly[Ordering[Monomial]], ImmutableArray.canBuildFrom))
  }

  def -(r: Polynomial): Polynomial = {
    new Polynomial(mergeWith(terms, r.terms)((a: Monomial, b: Monomial) => a - b)
      (implicitly[Ordering[Monomial]], ImmutableArray.canBuildFrom))
  }

  def unary_-(): Polynomial = {
    Polynomial(terms.map(-_)(breakOut))
  }

  def *(pp: PowerProduct): Polynomial = {
    Polynomial(terms.map(_ * pp)(breakOut))
  }

  def /(pp: PowerProduct): Polynomial = {
    Polynomial(terms.map(_ / pp)(breakOut))
  }


  def *(m: Monomial): Polynomial = {
    Polynomial(terms.map(_ * m)(breakOut))
  }

  def /(m: Monomial): Polynomial = {
    Polynomial(terms.map(_ / m)(breakOut))
  }

  def *(p: Polynomial): Polynomial = {
    Polynomial(groupRuns((for (i <- terms; j <- p.terms) yield i * j).sorted.iterator)(_.reduce(_ + _))
      (implicitly[Equiv[Monomial]], ImmutableArray.canBuildFrom))
  }

  def *(f: Rational): Polynomial = {
    Polynomial(terms.map(_ * f)(breakOut))
  }

  def /(f: Rational): Polynomial = {
    Polynomial(terms.map(_ / f)(breakOut))
  }

  def isZero = terms.isEmpty

  def nonZero = !isZero

  def isReducible(p: Polynomial): Boolean = {
    nonZero && p.nonZero && lt.isDivisibleBy(p.lt)
  }

  def reduce(p: Polynomial): Polynomial = {
    val q = lt / p.lt
    this - p * q
  }

  def spol(other: Polynomial): Polynomial = {
    val lpplcm = lpp lcm other.lpp
    (this * lpplcm / lt) - (other * lpplcm / other.lt)
  }

  def normalize(): Polynomial = {
    val coeff = lt.c
    if (coeff == Rational.one)
      return this
    this / coeff
  }
}
