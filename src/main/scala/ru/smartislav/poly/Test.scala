package ru.smartislav.poly

import spire.math.Rational

object Test extends App {
  val poly1 = Polynomial(
    Monomial("xxxyy"),
    Monomial(Rational(4, 5), "yzzz"))

  val poly2 = Polynomial(
    Monomial("yxx")
  )

  println(s"poly1: $poly1")
  println(s"poly2: $poly2")

  println(s"sum: ${poly1 + poly2}")
  println(s"difference: ${poly1 - poly2}")
  println(s"product: ${poly1 * poly2}")
  println(s"reduction by <poly2>: ${poly1.reduceByBasis(Seq(poly2))}")

  println(s"LT(poly1): ${poly1.lpp }")
  println(s"LT(poly2): ${poly2.lpp }")

  println(Monomial("xx") isDivisibleBy Monomial("x"))

  println(s"Gröbner basis for <poly1, poly2>: ${PolynomialBasis(poly1, poly2).gröbner() }")


  val a = Polynomial(
    Monomial("xy"),
    Monomial(2, "x"),
    Monomial(-1, "z"))
  val b = Polynomial(
    Monomial("xx"),
    Monomial(2, "y"),
    Monomial(-1, "z"))

  val basis = PolynomialBasis(a, b).gröbner().reduce()

  val c = Polynomial(
    Monomial(-1, "xz"),
    Monomial(-2, "yy"),
    Monomial("yz"),
    Monomial(-4, "y"),
    Monomial(2, "z")
  )

  println(s"Reduced Gröbner Basis: $basis")
}
