package ru.smartislav.poly.opt

import scala.collection.mutable
import ru.smartislav.poly
import scalaz.ImmutableArray

abstract class ExpressionContextBuilder[R] {
  private val varName = mutable.ArrayBuffer[String]()
  private val varIx = mutable.HashMap[String, Int]()

  protected final def variables(vs: Iterable[String]): Unit = {
    for (v <- vs; if !varIx.contains(v)) {
      varName += v
      varIx += v -> (varName.length - 1)
    }
  }

  protected final def variables(vs: String*): Unit = {
    variables(vs: Iterable[String])
  }

  protected final def monomials(ms: poly.Monomial*): Unit = {
    ms.foreach(m => variables(m.powers.keys))
  }

  protected final def polynomials(ps: poly.Polynomial*): Unit = {
    ps.foreach(p => p.monomials.foreach(m => monomials(m)))
  }

  def apply(): R = {
    val ctx = new ExpressionContext(varName.to[ImmutableArray])
    run(ctx)
  }

  protected def run(implicit ctx: ExpressionContext): R
}
