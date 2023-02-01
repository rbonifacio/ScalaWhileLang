package br.unb.cic.pca.df

import scala.collection.mutable.Map

import br.unb.cic.pca.Statement
import br.unb.cic.pca.*

object RD {
  type PS = Set[(Name, Label)] // property space (see Basic Definitions on page 67. we used to name this type
  // as "abstraction")

  type In = Map[Label, PS]
  type Out = Map[Label, PS]

  val UNDEF = 0

  def reachingDefinitions(p: Program): (In, Out) = {
    val in: In = Map[Label, PS]()
    val out: Out = Map[Label, PS]() // initialSet(p)

    for (l <- labels(p)) {
      out += l -> Set()
    }

    var fixed = false

    while (!fixed) {
      val temp = (in.clone(), out.clone())
      for (b <- blocks(p)) {

        val l = label(b)
        if (l == init(p)) {
          in += l -> fv(p).map(x => (x, UNDEF))
        }
        else {
           in += l -> flow(p).filter((source, target) => target == l).map((source, target) => out(source)).foldLeft(Set())(_ union _)
//          var acc: PS = Set[(Name, Label)]()
//          for((source, target) <- flow(p).filter((source, target) => target == l)) {
//            acc = acc union out(source)
//          }
//          in += l -> acc
        }
        out += l -> (gen(b) union (in(l) diff kill(b, p)))
      }
      fixed = (in, out) == temp
    }
    (in, out)
  }

  // def initialSet(p: Statement) : Out = (for { l <- labels(p) } yield l -> Set[(Name, Label)]()).toMap

  def gen(s: Block): PS = s match {
    case AssignmentStmt(x, _, l) => Set((x, l))
    case _ => Set()
  }

  def kill(s: Block, p: Program): PS = s match {
    case AssignmentStmt(x, _, l) => Set[(Name, Label)]((x, UNDEF)) union (assignments(p).filter(a => a.name == x && a.label != l).map(a => (a.name, a.label)))
    case _ => Set()
  }
}

