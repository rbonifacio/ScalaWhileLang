package br.unb.cic.pca

import org.scalatest.funsuite.AnyFunSuite

class CFGTest extends AnyFunSuite {

  /* factorial program example in page 4 (in terms of an AST) */
  val s1 = AssignmentStmt("y", VarExp("x"), 1)
  val s2 = AssignmentStmt("z", NumExp(1), 2)
  val s4 = AssignmentStmt("z", MulExp(VarExp("z"), VarExp("y")), 4)
  val s5 = AssignmentStmt("y", SubExp(VarExp("y"), NumExp(1)), 5)
  val s3 = WhileStmt(Condition(GTExp(VarExp("y"), NumExp(1)), 3), SequenceStmt(s4, s5))
  val s6 = AssignmentStmt("y", NumExp(0), 6)

  val s = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, s6)))

  test("Test init for assignment statement") {
    assert(1 == init(s1))
    assert(2 == init(s2))
    assert(4 == init(s4))
    assert(5 == init(s5))
    assert(6 == init(s6))
  }

  test("Test init for sequence statement") {
    assert(1 == init(s))
    assert(4 == init(SequenceStmt(s4, s5)))
  }

  test("Test init for while statement") {
    assert(3 == init(s3))
  }

  test("Test final for assignment statement") {
    assert(Set(1) == `final`(s1))
    assert(Set(2) == `final`(s2))
    assert(Set(4) == `final`(s4))
    assert(Set(5) == `final`(s5))
    assert(Set(6) == `final`(s6))
  }

  test("Test final for sequence statement") {
    assert(Set(6) == `final`(s))
    assert(Set(5) == `final`(SequenceStmt(s4, s5)))
  }

  test("Test final for while statement") {
    assert(Set(3) == `final`(s3))
  }

  test("Test cfg for program s") {
    assert(Set((1,2), (2,3), (3,4), (4,5), (5, 3), (3,6)) == flow(s))
  }
}
