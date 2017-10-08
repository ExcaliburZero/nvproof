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

case class ByDefModal(lineNumber: AST.LineNumber) extends Rule {
  override def toString(): String = f"$lineNumber by def modal"
}


object AST {
  type Proof = List[Step]
  type LineNumber = Int
}
