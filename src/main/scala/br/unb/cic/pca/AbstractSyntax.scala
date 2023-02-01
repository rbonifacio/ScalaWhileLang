package br.unb.cic.pca

type Program = Statement
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

def blocks(stmt: Statement): Set[Block] = stmt match {
  case AssignmentStmt(name, exp, label) => Set(AssignmentStmt(name, exp, label))
  case SkipStmt(label) => Set(SkipStmt(label))
  case SequenceStmt(s1, s2) => blocks(s1) union blocks(s2)
  case IfElseStmt(condition, thenStmt, elseStmt) => Set(condition) union blocks(thenStmt) union blocks(elseStmt)
  case WhileStmt(condition, stmt) => Set(condition) union blocks (stmt)
}

def assignments(stmt: Statement): Set[AssignmentStmt] = stmt match {
  case AssignmentStmt(name, exp, label) => Set(AssignmentStmt(name, exp, label))
  case SkipStmt(label) => Set()
  case SequenceStmt(s1, s2) => assignments(s1) union assignments(s2)
  case IfElseStmt(condition, thenStmt, elseStmt) => assignments(thenStmt) union assignments(elseStmt)
  case WhileStmt(condition, stmt) => assignments(stmt)
}

/* computes the free variables of a program */

def fv(exp: BooleanExpression): Set[Name] = exp match {
  case CTrue => Set()
  case CFalse => Set()
  case NotExp(exp) => fv(exp)
  case AndExp(left, right) => fv(left) union fv(right)
  case OrExp(left, right) => fv(left) union fv(right)
  case EqExp(left, right) => fv(left) union fv(right)
  case GTExp(left, right) => fv(left) union fv(right)
  case LTExp(left, right) => fv(left) union fv(right)
  case GEqExp(left, right) => fv(left) union fv(right)
  case LEqExp(left, right) => fv(left) union fv(right)
}

def fv(exp: ArithmeticExpression): Set[Name] = exp match {
  case VarExp(name) => Set(name)
  case NumExp(value) => Set()
  case AddExp(left, right) => fv(left) union fv(right)
  case SubExp(left, right) => fv(left) union fv(right)
  case MulExp(left, right) => fv(left) union fv(right)
  case DivExp(left, right) => fv(left) union fv(right)
}


def fv(stmt: Statement): Set[Name] = stmt match {
  case AssignmentStmt(name, exp, label) => fv(exp)
  case SkipStmt(label) => Set()
  case SequenceStmt(s1, s2) => fv(s1) union fv(s2)
  case IfElseStmt(Condition(c, _), thenStmt, elseStmt) => fv(c) union fv(thenStmt) union fv(elseStmt)
  case WhileStmt(Condition(c, _), stmt) => fv(c) union fv(stmt)
}

def complexExpressions(exp: BooleanExpression): Set[ArithmeticExpression] = exp match {
  case CTrue => Set()
  case CFalse => Set()
  case NotExp(exp) => complexExpressions(exp)
  case AndExp(left, right) => complexExpressions(left) union complexExpressions(right)
  case OrExp(left, right) => complexExpressions(left) union complexExpressions(right)
  case EqExp(left, right) => complexExpressions(left) union complexExpressions(right)
  case GTExp(left, right) => complexExpressions(left) union complexExpressions(right)
  case LTExp(left, right) => complexExpressions(left) union complexExpressions(right)
  case GEqExp(left, right) => complexExpressions(left) union complexExpressions(right)
  case LEqExp(left, right) => complexExpressions(left) union complexExpressions(right)
}
def complexExpressions(exp: ArithmeticExpression): Set[ArithmeticExpression] = exp match {
  case VarExp(name) => Set()
  case NumExp(value) => Set()
  case AddExp(left, right) => Set(AddExp(left, right)) union complexExpressions(left) union complexExpressions(right)
  case SubExp(left, right) => Set(AddExp(left, right)) union complexExpressions(left) union complexExpressions(right)
  case MulExp(left, right) => Set(AddExp(left, right)) union complexExpressions(left) union complexExpressions(right)
  case DivExp(left, right) => Set(AddExp(left, right)) union complexExpressions(left) union complexExpressions(right)
}

def complexExpressions(c: Condition): Set[ArithmeticExpression] = complexExpressions(c.exp) 

def complexExpressions(stmt: Statement): Set[ArithmeticExpression] = stmt match {
  case AssignmentStmt(name, exp, label) => complexExpressions(exp)
  case SkipStmt(label) => Set()
  case SequenceStmt(s1, s2) => complexExpressions(s1) union complexExpressions(s2)
  case IfElseStmt(condition, s1, s2) => complexExpressions(condition) union complexExpressions(s1) union complexExpressions(s2)
  case WhileStmt(condition, stmt) => complexExpressions(condition) union complexExpressions(stmt)
}



