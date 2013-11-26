package ru.smartislav.poly.opt

import org.scalacheck.Prop
import spire.math.Rational
import ru.smartislav.SpecBase
import ru.smartislav.poly

class MonomialSpec extends SpecBase {
  "Addition and subtraction work for all similar monomials" ! Prop.forAll(monomial, monomial) { (aa, bb) =>
    new ExpressionContextBuilder[Prop] {
      monomials(aa, bb)
      protected def run(implicit ctx: ExpressionContext): Prop = {
        val a = Monomial(aa)
        val b = Monomial(bb)

        a.isSimilarTo(b) ==> {
          (a + b).mustEqual(b + a)
          ((a + b) - b).mustEqual(a)
          (a + Monomial.zero).mustEqual(a)
          (a - b).mustEqual(-(b - a))
        }
      }
    }.apply()
  }

  "Monomial is always similar to itself" ! Prop.forAll(monomial) { (a) =>
    a isSimilarTo a
  }

  "Non-zero monomial is always divisible by itself" ! Prop.forAll(nonZeroMonomial) { (a) =>
    (a isDivisibleBy a) must beTrue
    (a / a).mustEqual(poly.Monomial.one)
  }

  "Multiplication and division cancel each other out" ! Prop.forAll(monomial, monomial) { (a, b) =>
    a.nonZero ==> (a * b / a).mustEqual(b)
    b.nonZero ==> (a * b / b).mustEqual(a)
    a.nonZero ==> (a / a).mustEqual(poly.Monomial.one)
  }

  "lcm(a, b) is always divisible by both a and b" ! Prop.forAll(nonZeroMonomial, nonZeroMonomial) { (a, b) =>
    val lcm = a lcm b
    (lcm isDivisibleBy a) must beTrue
    (lcm isDivisibleBy b) must beTrue
  }

  //  "lcm(a, b) is divisible by b" ! {
  //    val a = Monomial.one
  //    val b = Monomial(2, "uvvvwwwwwwwwwwwwxyyyyyyyyyyyyyyy")
  //    val lcm = a lcm b
  //    lcm mustEqual b
  //    (lcm isDivisibleBy b) must beTrue
  //  }

  "Scaling works for all monomials" ! Prop.forAll(monomial) { (a) =>
    (a * 2).mustEqual(a + a)
    (a * 1).mustEqual(a)
    (a * 0).mustEqual(poly.Monomial.zero)
    (a * -1).mustEqual(-a)
    (a * Rational(1, 2)).mustEqual(a / 2)
    (a / 1).mustEqual(a)
    (a / -1).mustEqual(-a)
  }

  //  "Pure lexicographical ordering" ! Prop.forAll(Gen.listOf(monomial)) { (ms) =>
  //
  //  }
}
