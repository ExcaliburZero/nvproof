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

  private def getStep(proof: AST.Proof, lineNumber: AST.LineNumber): Step = {
    proof(lineNumber - 1)
  }
}
