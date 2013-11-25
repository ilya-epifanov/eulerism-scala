package ru.smartislav.poly.opt

import scalaz.ImmutableArray
import scala.collection._
import scala.Predef.Map

case class PowerProduct(factors: ImmutableArray[Int]) extends AnyVal {
  def *(p: PowerProduct): PowerProduct = {
    new PowerProduct((factors zip p.factors).map({ case (a, b) => a + b })(breakOut))
  }

  def isDivisibleBy(p: PowerProduct): Boolean = {
    (factors zip p.factors).forall({ case (a, b) => a > b })
  }

  def /(p: PowerProduct): PowerProduct = {
    new PowerProduct((factors zip p.factors).map({ case (a, b) => a - b })(breakOut))
  }

  def gcd(p: PowerProduct): PowerProduct = {
    new PowerProduct((factors zip p.factors).map({ case (a, b) => math.min(a, b) })(breakOut))
  }

  def lcm(p: PowerProduct): PowerProduct = {
    new PowerProduct((factors zip p.factors).map({ case (a, b) => math.max(a, b) })(breakOut))
  }

  def coprime(p: PowerProduct): Boolean = {
    (factors zip p.factors).forall({ case (a, b) => a == 0 || b == 0 })
  }
}

object PowerProduct {
  def apply(powers: Map[String, Int])(implicit ctx: ExpressionContext): PowerProduct = {
    new PowerProduct((0 until ctx.length).map(ix => powers(ctx.varName(ix)))(breakOut))
  }

  object AscPurelexOrdering extends Ordering[PowerProduct] {
    def compare(x: PowerProduct, y: PowerProduct): Int = {
      Ordering.Iterable[Int].compare(x.factors, y.factors)
    }
  }

  implicit val PurelexOrdering = AscPurelexOrdering.reverse
}
