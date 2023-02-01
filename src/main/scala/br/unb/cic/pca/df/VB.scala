package br.unb.cic.pca.df

import br.unb.cic.pca.*

import scala.collection.mutable.Map

object VM {
  type PS = Set[ArithmeticExpression]

  type In = Map[Label, PS]
  type Out = Map[Label, PS]


  def veryBusyExpressions(p: Program): (In, Out) = {
    val in: In = Map[Label, PS]()
    val out: Out = Map[Label, PS]()

    for (l <- labels(p)) {
      in += l -> complexExpressions(p)
    }

    var fixed = false

    while (!fixed) {
      val temp = (in.clone(), out.clone())

      for (b <- blocks(p)) {
        val l = label(b)
        if (`final`(p).contains(l)) {
          out += l -> Set()
        }
        else {
          out += l -> flowR(p).filter((source, target) => target == l).map((source, target) => in(source)).foldLeft(complexExpressions(p))(_ intersect _)
        }
        in += l -> (gen(b) union (out(l) diff kill(b, p)))
      }
      fixed = (in, out) == temp
    }
    (in, out)
  }

  def gen(b: Block): PS = b match {
    case AssignmentStmt(name, exp, label) => complexExpressions(exp)
    case SkipStmt(_) => Set()
    case Condition(exp, label) => complexExpressions(exp)
  }

  def kill(b: Block, p: Program): PS = b match {
    case AssignmentStmt(name, exp, label) => complexExpressions(p).filter(exp => fv(exp).contains(name))
    case _ => Set()
  }
}

