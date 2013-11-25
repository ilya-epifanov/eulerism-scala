package ru.smartislav.poly

import scala.annotation.tailrec
import scalaz.{Cord, Show}
import spire.math.Rational
import ru.smartislav.eulerism._

class Polynomial private(val monomials: Seq[Monomial]) {
  def lm = monomials.head

  lazy val lpp = lm.powerProduct

  def +(r: Polynomial): Polynomial = {
    implicit val ord = Monomial.DefaultOrdering
    Polynomial(mergeWith(monomials, r.monomials)((a, b) => a + b): _*)
  }

  def -(r: Polynomial): Polynomial = {
    this + -r
  }

  def unary_-(): Polynomial = {
    Polynomial(monomials map (-_): _*)
  }

  def *(m: Monomial): Polynomial = {
    Polynomial(monomials map (_ * m): _*)
  }

  def *(p: Polynomial): Polynomial = {
    Polynomial((for (i <- monomials; j <- p.monomials) yield i * j): _*)
  }

  def *(f: Rational): Polynomial = {
    Polynomial(monomials map (_ * f): _*)
  }

  def /(m: Monomial): Polynomial = {
    Polynomial(monomials map (_ / m): _*)
  }

  def /(f: Rational): Polynomial = {
    Polynomial(monomials map (_ / f): _*)
  }

  def isReducible(p: Polynomial): Boolean = {
    nonZero && p.nonZero && lm.isDivisibleBy(p.lm)
  }

  def reduce(p: Polynomial): Polynomial = {
    val q = lm / p.lm
    this - p * q
  }

  @tailrec
  final def reduceByBasis(basis: Seq[Polynomial]): Polynomial = {
    if (isZero) return this
    basis.find(isReducible) match {
      case Some(p) => reduce(p).reduceByBasis(basis)
      case None => this
    }
  }

  def sPoly(other: Polynomial): Polynomial = {
    val lpplcm = lpp lcm other.lpp
    (this * lpplcm / lm) - (other * lpplcm / other.lm)
  }

  def normalize(): Polynomial = {
    val coeff = lm.c
    if (coeff == Rational.one)
      return this
    this / coeff
  }

  def isZero = monomials.isEmpty

  def nonZero = !isZero

  override def equals(that: Any): Boolean = {
    if (!that.isInstanceOf[Polynomial]) false
    else monomials == that.asInstanceOf[Polynomial].monomials
  }

  override def hashCode(): Int = monomials.hashCode()

  override def toString: String = Polynomial.DebugShow.shows(this)
}

object Polynomial {
  val zero = new Polynomial(Seq.empty)
  val one = Polynomial(Monomial.one)

  def apply(ms: Monomial*): Polynomial = {
    implicit val ord = Monomial.DefaultOrdering
    ordered(groupRuns(ms.sorted.iterator)(_.reduce(_ + _)))
  }

  def ordered(ms: Seq[Monomial]): Polynomial = {
    val nonZeroMs = ms.filter(_.nonZero)
    if (nonZeroMs.isEmpty) zero
    else new Polynomial(nonZeroMs)
  }

  implicit object DebugShow extends Show[Polynomial] {
    override def show(p: Polynomial): Cord = {
      if (p.monomials.isEmpty) {
        Cord("0")
      } else {
        Cord.mkCord(" + ", (p.monomials map Monomial.DebugShow.show).toSeq: _*)
      }
    }
  }

  implicit object DefaultOrdering extends Ordering[Polynomial] {
    def compare(x: Polynomial, y: Polynomial): Int = {
      Ordering.Iterable[Monomial](Monomial.DefaultExactOrdering).compare(x.monomials, y.monomials)
    }
  }

}
