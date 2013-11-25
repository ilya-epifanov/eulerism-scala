package ru.smartislav.poly.opt

import scalaz.ImmutableArray
import scala.collection.immutable.HashMap

class ExpressionContext(val varName: ImmutableArray[String]) {
  val varIx = HashMap(varName.zipWithIndex: _*)
  val length = varName.length
}
