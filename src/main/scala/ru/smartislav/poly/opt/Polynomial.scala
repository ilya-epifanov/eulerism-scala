package ru.smartislav.poly.opt

import scalaz.ImmutableArray
import ru.smartislav.eulerism._
import scala.collection._
import spire.math.Rational
import scala.annotation.tailrec
import ru.smartislav.poly

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
    Polynomial(terms.map(-_)(breakOut): ImmutableArray[Monomial])
  }

  def *(pp: PowerProduct): Polynomial = {
    Polynomial(terms.map(_ * pp)(breakOut): ImmutableArray[Monomial])
  }

  def /(pp: PowerProduct): Polynomial = {
    Polynomial(terms.map(_ / pp)(breakOut): ImmutableArray[Monomial])
  }


  def *(m: Monomial): Polynomial = {
    Polynomial(terms.map(_ * m)(breakOut): ImmutableArray[Monomial])
  }

  def /(m: Monomial): Polynomial = {
    Polynomial(terms.map(_ / m)(breakOut): ImmutableArray[Monomial])
  }

  def *(p: Polynomial): Polynomial = {
    Polynomial(groupRuns((for (i <- terms; j <- p.terms) yield i * j).sorted.iterator)(_.reduce(_ + _))
      (implicitly[Equiv[Monomial]], ImmutableArray.canBuildFrom))
  }

  def *(f: Rational): Polynomial = {
    Polynomial(terms.map(_ * f)(breakOut): ImmutableArray[Monomial])
  }

  def /(f: Rational): Polynomial = {
    Polynomial(terms.map(_ / f)(breakOut): ImmutableArray[Monomial])
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

  @tailrec
  final def reduce(b: PolynomialBasis): Polynomial = {
    if (isZero) return this
    b.dims.find(isReducible) match {
      case Some(p) => reduce(p).reduce(b)
      case None => this
    }
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

object Polynomial {
  implicit def apply(m: poly.Polynomial)(implicit ctx: ExpressionContext): Polynomial = {
    Polynomial(m.monomials.map(Monomial.apply).sorted.to(ImmutableArray.canBuildFrom))
  }
}