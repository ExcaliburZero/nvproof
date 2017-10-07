package nvproof

case class Step(lineNumber: AST.LineNumber, statement: Statement, rule: Rule)

sealed trait Statement
case class UnaryExpression(unaryOperator: UnaryOperator, statement: Statement) extends Statement
case class BinaryExpression(statement1: Statement, binaryOperator: BinaryOperator, statement2: Statement) extends Statement
case class Symbol(char: Char) extends Statement

sealed trait UnaryOperator
case class Not() extends UnaryOperator
case class Necessary() extends UnaryOperator
case class Possible() extends UnaryOperator

sealed trait BinaryOperator
case class Implication() extends BinaryOperator

sealed trait Rule
case class Assumption() extends Rule
case class ModusPonens(lineNumber1: AST.LineNumber, lineNumber2: AST.LineNumber) extends Rule

object AST {
  type Proof = List[Step]
  type LineNumber = Int
}
