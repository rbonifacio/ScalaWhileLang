package br.unb.cic.pca.df

import br.unb.cic.pca.{AssignmentStmt, Condition, GTExp, MulExp, NumExp, SequenceStmt, SubExp, VarExp, WhileStmt}
import br.unb.cic.pca.df.RD.*
import org.scalatest.funsuite.AnyFunSuite

import java.util

class RDTest extends AnyFunSuite
{

  /* factorial program example in page 4 (in terms of an AST) */
  val s1 = AssignmentStmt("y", VarExp("x"), 1)
  val s2 = AssignmentStmt("z", NumExp(1), 2)
  val s4 = AssignmentStmt("z", MulExp(VarExp("z"), VarExp("y")), 4)
  val s5 = AssignmentStmt("y", SubExp(VarExp("y"), NumExp(1)), 5)
  val s3 = WhileStmt(Condition(GTExp(VarExp("y"), NumExp(1)), 3), SequenceStmt(s4, s5))
  val s6 = AssignmentStmt("y", NumExp(0), 6)

  val s = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, s6)))

  val x = "x"
  val y = "y"
  val z = "z"

  test("Testing reaching definition on the factorial program s") {
    println(reachingDefinitions(s))
  }
}
