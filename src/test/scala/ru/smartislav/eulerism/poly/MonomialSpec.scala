package ru.smartislav.eulerism.poly

import org.scalacheck.Prop
import ru.smartislav.eulerism.scala.poly.Monomial
import spire.math.Rational
import ru.smartislav.eulerism.SpecBase

class MonomialSpec extends SpecBase {
  "Addition and subtraction work for all similar monomials" ! Prop.forAll(monomial, monomial) { (a, b) =>
    a.isSimilarTo(b) ==> {
      (a + b).mustEqual(b + a)
      ((a + b) - b).mustEqual(a)
      (a + Monomial.zero).mustEqual(a)
      (a - b).mustEqual(-(b - a))
    }
  }

  "Monomial is always similar to itself" ! Prop.forAll(monomial) { (a) =>
    a isSimilarTo a
  }

  "Non-zero monomial is always divisible by itself" ! Prop.forAll(nonZeroMonomial) { (a) =>
    (a isDivisibleBy a) must beTrue
    (a / a).mustEqual(Monomial.one)
  }

  "Multiplication and division cancel each other out" ! Prop.forAll(monomial, monomial) { (a, b) =>
    a.nonZero ==> (a * b / a).mustEqual(b)
    b.nonZero ==> (a * b / b).mustEqual(a)
    a.nonZero ==> (a / a).mustEqual(Monomial.one)
  }

  "lcm(a, b) is always divisible by both a and b" ! Prop.forAll(nonZeroMonomial, nonZeroMonomial) { (a, b) =>
    val lcm = a lcm b
    (lcm isDivisibleBy a) must beTrue
    (lcm isDivisibleBy b) must beTrue
  }

  "lcm(a, b) is divisible by b" ! {
    val a = Monomial.one
    val b = Monomial(Rational(2),
      "u" -> Rational(1, 10),
      "v" -> Rational(3, 10),
      "w" -> Rational(5, 4),
      "x" -> Rational(3, 7),
      "y" -> Rational(5))
    //    2 * u ^ (1 / 10) * v ^ (3 / 10) * w ^ (5 / 4) * x ^ (3 / 7) * y ^ 5
    val lcm = a lcm b
    lcm mustEqual b
    (lcm isDivisibleBy b) must beTrue
  }

  "Scaling works for all monomials" ! Prop.forAll(monomial) { (a) =>
    (a * 2).mustEqual(a + a)
    (a * 1).mustEqual(a)
    (a * 0).mustEqual(Monomial.zero)
    (a * -1).mustEqual(-a)
    (a * Rational(1, 2)).mustEqual(a / 2)
    (a / 1).mustEqual(a)
    (a / -1).mustEqual(-a)
  }

  //  "Pure lexicographical ordering" ! Prop.forAll(Gen.listOf(monomial)) { (ms) =>
  //
  //  }
}
