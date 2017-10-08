package nvproof

case class Step(lineNumber: AST.LineNumber, statement: Statement, rule: Rule) {
  override def toString(): String = f"$lineNumber) $statement $rule"
}


sealed trait Statement
case class UnaryExpression(unaryOperator: UnaryOperator, statement: Statement) extends Statement {
  override def toString(): String = f"$unaryOperator$statement"
}
case class BinaryExpression(statement1: Statement, binaryOperator: BinaryOperator, statement2: Statement) extends Statement {
  override def toString(): String = f"($statement1 $binaryOperator $statement2)"
}

case class Symbol(char: Char) extends Statement {
  override def toString(): String = char.toString
}

sealed trait UnaryOperator
case class Not() extends UnaryOperator {
  override def toString(): String = "~"
}
case class Necessary() extends UnaryOperator {
  override def toString(): String = "□"
}
case class Possible() extends UnaryOperator {
  override def toString(): String = "⋄"
}

sealed trait BinaryOperator
case class Implication() extends BinaryOperator {
  override def toString(): String = "⇒"
}


sealed trait Rule
case class Assumption() extends Rule {
  override def toString(): String = "AS"
}

case class ModusPonens(lineNumber1: AST.LineNumber, lineNumber2: AST.LineNumber) extends Rule {
  override def toString(): String = f"MP $lineNumber1, $lineNumber2"
}

case class Contraposition(lineNumber: AST.LineNumber) extends Rule {
  override def toString(): String = f"Contra $lineNumber"
}

case class DoubleNegation(lineNumber: AST.LineNumber) extends Rule {
  override def toString(): String = f"DN* $lineNumber"
}

case class L1() extends Rule {
  override def toString(): String = f"L1"
}

case class L2() extends Rule {
  override def toString(): String = f"L2"
}

case class L3() extends Rule {
  override def toString(): String = f"L3"
}

case class M1() extends Rule {
  override def toString(): String = f"M1"
}

case class M2() extends Rule {
  override def toString(): String = f"M2"
}

case class M3() extends Rule {
  override def toString(): String = f"M3"
}

case class M4() extends Rule {
  override def toString(): String = f"M4"
}

case class M5() extends Rule {
  override def toString(): String = f"M5"
}

case class Necessitation(lineNumber: AST.LineNumber) extends Rule {
  override def toString(): String = f"Necess $lineNumber"
}

case class ByDefModal(lineNumber: AST.LineNumber) extends Rule {
  override def toString(): String = f"$lineNumber by def modal"
}


object AST {
  type Proof = List[Step]
  type LineNumber = Int

  def sourcePrint(proof: Proof): String = {
    (for (step <- proof) yield printStep(step)).mkString("\n") + "\n"
  }

  private def printStep(step: Step): String = {
    val lineNumber = step.lineNumber
    val statement = step.statement
    val rule = step.rule
    f"$lineNumber) " + printStatement(statement) + " " + printRule(rule)
  }

  private def printStatement(statement: Statement): String = {
    statement match {
      case Symbol(a) =>
        a.toString
      case UnaryExpression(op, st) =>
        printUnaryOperator(op) + printStatement(st)
      case BinaryExpression(st1, op, st2) =>
        "(" + printStatement(st1) + " " + printBinaryOperator(op) + " " + printStatement(st2) + ")"
    }
  }

  private def printUnaryOperator(op: UnaryOperator): String = {
    op match {
      case Not() => "~"
      case Necessary() => "[]"
      case Possible() => "<>"
    }
  }

  private def printBinaryOperator(op: BinaryOperator): String = {
    op match {
      case Implication() => "->"
    }
  }

  private def printRule(rule: Rule): String = {
    rule match {
      case Assumption() => "AS"
      case ModusPonens(ln1, ln2) => f"MP $ln1, $ln2"
      case Contraposition(ln1) => f"Contra $ln1"
      case DoubleNegation(ln1) => f"DN* $ln1"
      case L1() => "L1"
      case L2() => "L2"
      case L3() => "L3"
      case M1() => "M1"
      case M2() => "M2"
      case M3() => "M3"
      case M4() => "M4"
      case M5() => "M5"
      case Necessitation(ln1) => f"Necess $ln1"
      case ByDefModal(ln1) => f"$ln1 by def modal"
    }
  }

  def isTheorem(proof: Proof, ln: LineNumber): Boolean = {
    val rule = proof(ln - 1).rule
    rule match {
      case Assumption() => false
      case ModusPonens(ln1, ln2) =>
        isTheorem(proof, ln1) && isTheorem(proof, ln2)
      case Contraposition(ln1) =>
        isTheorem(proof, ln1)
      case DoubleNegation(ln1) =>
        isTheorem(proof, ln1)
      case L1() => true
      case L2() => true
      case L3() => true
      case M1() => true
      case M2() => true
      case M3() => true
      case M4() => true
      case M5() => true
      case Necessitation(ln1) =>
        isTheorem(proof, ln1)
      case ByDefModal(ln1) =>
        isTheorem(proof, ln1)
    }
  }
}
