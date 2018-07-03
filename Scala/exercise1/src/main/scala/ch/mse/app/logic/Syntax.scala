package ch.mse.app.logic

abstract class BoolOperator 
case object CondAnd extends BoolOperator 
case object CondOr extends BoolOperator 

abstract class BoolExpr
case class LitBExpr(value: Boolean) extends BoolExpr
case class NegBExpr(expr: BoolExpr) extends BoolExpr
case class DyaBExpr(operator: BoolOperator, op1: BoolExpr, op2: BoolExpr) extends BoolExpr
