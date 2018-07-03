package ch.mse.app.logic

import scala.collection.Map

object Interpreter {
  
  type Value = Int
  type State = Map[String, Value]

  def eval(expr: BoolExpr, state: State): Boolean =
    expr match {
      case LitBExpr(value) => value
      case NegBExpr(expr) => !eval(expr, state)
      case DyaBExpr(CondAnd, op1, op2) => eval(op1, state) && eval(op2, state)
      case DyaBExpr(CondOr, op1, op2) => eval(op1, state) || eval(op2, state)
    }

}