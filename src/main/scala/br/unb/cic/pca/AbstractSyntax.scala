package br.unb.cic.pca

type Label = Integer
type Name = String

sealed trait Block

/* Statements */
sealed trait Statement

case class AssignmentStmt(name: Name, exp: ArithmeticExpression, label: Label) extends Statement with Block
case class SkipStmt(label: Label) extends Statement with Block
case class SequenceStmt(s1: Statement, s2: Statement) extends Statement
case class IfElseStmt(condition: Condition, thenStmt: Statement, elseStmt: Statement) extends Statement
case class WhileStmt(condition: Condition, stmt: Statement) extends Statement

case class Condition(exp: BooleanExpression, label: Label) extends Block

/* Expressions */
sealed trait Expression


/* Arithmetic expressions */
sealed trait ArithmeticExpression extends Expression

case class VarExp(name: Name) extends ArithmeticExpression
case class NumExp(value: Integer) extends ArithmeticExpression
case class AddExp(left: ArithmeticExpression, right: ArithmeticExpression) extends ArithmeticExpression
case class SubExp(left: ArithmeticExpression, right: ArithmeticExpression) extends ArithmeticExpression
case class MulExp(left: ArithmeticExpression, right: ArithmeticExpression) extends ArithmeticExpression
case class DivExp(left: ArithmeticExpression, right: ArithmeticExpression) extends ArithmeticExpression

/* Boolean expressions */
sealed trait BooleanExpression extends Expression

case object CTrue extends BooleanExpression
case object CFalse extends BooleanExpression

case class NotExp(exp: BooleanExpression) extends BooleanExpression
case class AndExp(left: BooleanExpression, right: BooleanExpression) extends BooleanExpression
case class OrExp(left: BooleanExpression, right: BooleanExpression) extends BooleanExpression

/* Relational expressions */
case class EqExp(left: ArithmeticExpression, right: ArithmeticExpression) extends BooleanExpression
case class GTExp(left: ArithmeticExpression, right: ArithmeticExpression) extends BooleanExpression
case class LTExp(left: ArithmeticExpression, right: ArithmeticExpression) extends BooleanExpression
case class GEqExp(left: ArithmeticExpression, right: ArithmeticExpression) extends BooleanExpression
case class LEqExp(left: ArithmeticExpression, right: ArithmeticExpression) extends BooleanExpression


