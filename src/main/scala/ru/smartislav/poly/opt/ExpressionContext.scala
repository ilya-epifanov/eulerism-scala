package ru.smartislav.poly.opt

import scalaz.ImmutableArray
import scala.collection.immutable.HashMap

class ExpressionContext(val varName: ImmutableArray[String]) {
  val varIx = HashMap(varName.zipWithIndex: _*)
  val length = varName.length

  def apply(name: String): Int = varIx(name)

  def apply(ix: Int): String = varName(ix)

  def powerProduct(factors: (String, Int)*): PowerProduct = {
    val arr = new Array[Int](length)
    for ((v, p) <- factors)
      arr.update(varIx(v), p)
    PowerProduct(ImmutableArray.fromArray(arr))
  }
}
