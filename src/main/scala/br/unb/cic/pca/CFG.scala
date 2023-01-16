package br.unb.cic.pca

type Edge = (Label, Label)

import br.unb.cic.pca._

/**
 * Computes the initial label of a statement.
 */
def init(stmt: Statement): Label = stmt match {
  case AssignmentStmt(_, _, label) => label
  case SkipStmt(label) => label
  case SequenceStmt(s1, _) => init(s1)
  case IfElseStmt(Condition(_, label), _, _) => label
  case WhileStmt(Condition(_, label), stmt) => label
}

/**
 * Computes the final label of a statement.
 */
def `final`(stmt: Statement): Set[Label] = stmt match {
  case AssignmentStmt(name, _, label) => Set(label)
  case SkipStmt(label) => Set(label)
  case SequenceStmt(s1, s2) => `final`(s2)
  case IfElseStmt(_, s1, s2) => `final`(s1) union `final`(s2)
  case WhileStmt(Condition(_, label), stmt) => Set(label)
}

def blocks(stmt: Statement): Set[Block] = stmt match {
  case AssignmentStmt(name, exp, label) => Set(AssignmentStmt(name, exp, label))
  case SkipStmt(label) => Set(SkipStmt(label))
  case SequenceStmt(s1, s2) => blocks(s1) union blocks(s2)
  case IfElseStmt(condition, thenStmt, elseStmt) => Set(condition) union blocks(thenStmt) union blocks(elseStmt)
  case WhileStmt(condition, stmt) => Set(condition) union blocks (stmt)
}

/**
 * Computes the label of a given block.
 */
def label(block: Block): Label = block match {
  case AssignmentStmt(_, _, label) => label
  case SkipStmt(label) => label
  case Condition(exp, label) => label
}

def labels(stmt: Statement): Set[Label] = blocks(stmt).map(b => label(b))

def flow(stmt: Statement): Set[Edge] = stmt match {
  case AssignmentStmt(_, _, label) => Set()
  case SkipStmt(label) => Set()
  case SequenceStmt(s1, s2) => flow(s1) union flow(s2) union (for(l1 <- `final`(s1)) yield (l1, init(s2)))
  case IfElseStmt(Condition(_, l), s1, s2) => flow(s1) union flow(s2) union Set((l, init(s1)), (l, init(s2)))
  case WhileStmt(Condition(_, l1), stmt) => flow(stmt) union Set((l1, init(stmt))) union  (for (l2 <- `final`(stmt)) yield (l2, l1))
}