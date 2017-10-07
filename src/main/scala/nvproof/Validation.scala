package nvproof

case class ErrorMessage(msg: String)

object Validation {
  def validate(proof: AST.Proof): Option[List[ErrorMessage]] = {
    val possibleErrors = for (step <- proof) yield validateStep(proof, step)
    val actualErrors = possibleErrors.flatten

    if (actualErrors.nonEmpty) {
      Some(actualErrors)
    } else {
      None
    }
  }

  private def validateStep(proof: AST.Proof, step: Step): Option[ErrorMessage] = {
    val statement = step.statement
    val rule = step.rule

    rule match {
      case Assumption() => None
      case ModusPonens(ln1, ln2) => validateModusPonens(proof, step, ln1, ln2)
      case Contraposition(ln) => validateContraposition(proof, step, ln)
      case DoubleNegation(ln) => validateDoubleNegation(proof, step, ln)
      case M1() => validateM1(step)
      case ByDefModal(ln) => validateByDefModal(proof, step, ln)
    }
  }

  def validateModusPonens(proof: AST.Proof, step: Step, ln1: AST.LineNumber, ln2: AST.LineNumber): Option[ErrorMessage] = {
    val l1 = getStep(proof, ln1).statement
    val l2 = getStep(proof, ln2).statement

    val ln = step.lineNumber
    val statement = step.statement
    val start = f"Invalid MP on line $ln: "

    l1 match {
      case BinaryExpression(a, Implication(), b) =>
        if (a == l2) {
          if (b == statement) {
            None
          } else {
            Some(ErrorMessage(start + f"second operand on line $ln1 ($b) != line $ln ($statement)"))
          }
        } else {
          Some(ErrorMessage(start + f"first operand on line $ln1 ($a) != line $ln2 ($l2)"))
        }
      case _ =>
        Some(ErrorMessage(start + f"line $ln1 does not have implication as its primary operation: $l1"))
    }
  }

  def validateContraposition(proof: AST.Proof, step: Step, ln1: AST.LineNumber): Option[ErrorMessage] = {
    val l1 = getStep(proof, ln1).statement

    val ln = step.lineNumber
    val statement = step.statement
    val start = f"Invalid Contra on line $ln: "

    l1 match {
      case BinaryExpression(a, Implication(), b) =>
        statement match {
          case BinaryExpression(UnaryExpression(Not(), a2), Implication(), UnaryExpression(Not(), b2)) =>
            if (a == b2) {
              if (b == a2) {
                None
              } else {
                Some(ErrorMessage(start + f"second operand on line $ln1 ($b) != un-negated first operand on line $ln ($a2)"))
              }
            } else {
              Some(ErrorMessage(start + f"first operand on line $ln1 ($a) != un-negated second operand on line $ln ($b2)"))
            }
          case _ =>
            Some(ErrorMessage(start + f"line $ln does not have the correct structure for contraposition: $statement"))
        }
      case _ =>
        Some(ErrorMessage(start + f"line $ln1 does not have implication as its primary operation: $l1"))
    }
  }

  def validateDoubleNegation(proof: AST.Proof, step: Step, ln1: AST.LineNumber): Option[ErrorMessage] = {
    val l1 = getStep(proof, ln1).statement

    val ln = step.lineNumber
    val statement = step.statement
    val start = f"Invalid DN* on line $ln: "

    val statementNoDN = removeDoubleNegations(statement)
    val l1NoDN = removeDoubleNegations(l1)

    if (statementNoDN == l1NoDN) {
      None
    } else {
      Some(ErrorMessage(start + f"line $ln is not equivalent to line $ln1 through DN*"))
    }
  }

  def validateM1(step: Step): Option[ErrorMessage] = {
    val ln = step.lineNumber
    val statement = step.statement
    val start = f"Invalid M1 on line $ln: "

    statement match {
      case BinaryExpression(a, Implication(), b) =>
        a match {
          case UnaryExpression(Necessary(), a2) =>
            if (a2 == b) {
              None
            } else {
              Some(ErrorMessage(start + f"first operand on line $ln without necessitation ($a2) != second operand on line $ln ($b)"))
            }
          case _ =>
            Some(ErrorMessage(start + f"first operand on line $ln is does not have necessitation ($a)"))
        }
      case _ =>
        Some(ErrorMessage(start + f"line $ln does not have the correct structure for an instance of M1"))
    }
  }

  def validateByDefModal(proof: AST.Proof, step: Step, ln1: AST.LineNumber): Option[ErrorMessage] = {
    val l1 = getStep(proof, ln1).statement

    val ln = step.lineNumber
    val statement = step.statement
    val start = f"Invalid by def modal on line $ln: "

    val statementNoPossible = removePossible(statement)
    val l1NoPossible = removePossible(l1)

    if (statementNoPossible == l1NoPossible) {
      None
    } else {
      Some(ErrorMessage(start + f"line $ln is not equivalent to line $ln1 through by def modal"))
    }
  }

  private def getStep(proof: AST.Proof, lineNumber: AST.LineNumber): Step = {
    proof(lineNumber - 1)
  }

  private def removeDoubleNegations(statement: Statement): Statement = {
    statement match {
      case UnaryExpression(Not(), UnaryExpression(Not(), a)) =>
        removeDoubleNegations(a)
      case UnaryExpression(op, a) =>
        UnaryExpression(op, removeDoubleNegations(a))
      case BinaryExpression(a, op, b) =>
        BinaryExpression(removeDoubleNegations(a), op, removeDoubleNegations(b))
      case a => a
    }
  }

  private def removePossible(statement: Statement): Statement = {
    statement match {
      case UnaryExpression(Not(), UnaryExpression(Possible(), UnaryExpression(Not(), a))) =>
        UnaryExpression(Necessary(), removePossible(a))
      case UnaryExpression(Possible(), a) =>
        UnaryExpression(Not(), UnaryExpression(Necessary(), UnaryExpression(Not(), a)))
      case UnaryExpression(op, a) =>
        UnaryExpression(op, removePossible(a))
      case BinaryExpression(a, op, b) =>
        BinaryExpression(removePossible(a), op, removePossible(b))
      case a => a
    }
  }
}
