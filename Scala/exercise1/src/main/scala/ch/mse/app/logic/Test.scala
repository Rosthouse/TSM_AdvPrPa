package ch.mse.app.logic

import Interpreter.eval
import scala.collection.mutable.HashMap

object Test {

  def main(args: Array[String]) {
    val bExpr = DyaBExpr(CondAnd, LitBExpr(true), LitBExpr(false));
    println(eval(bExpr, new HashMap[String, Int]()));
  }

}