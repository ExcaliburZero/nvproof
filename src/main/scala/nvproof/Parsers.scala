package nvproof

import scala.util.parsing.combinator._

object Parsers extends RegexParsers {
  override val skipWhitespace = false

  def step: Parser[Step] = {
    lineNumber ~ ") " ~ statement ~ " " ~ rule ^^ {
      case ln ~ _ ~ st ~ _ ~ rl => Step(ln, st, rl)
    }
  }

  def lineNumber: Parser[AST.LineNumber] = {
    "[0-9]+".r ^^ { str => str.toInt }
  }

  def statement: Parser[Statement] = {
    unaryExpression | symbol
  }

  def unaryExpression: Parser[UnaryExpression] = {
    unaryOperator ~ statement ^^ {
      case op ~ st => UnaryExpression(op, st)
    }
  }

  def symbol: Parser[Statement] = {
    "[A-Z]".r ^^ { char => Symbol(char.charAt(0)) }
  }

  def unaryOperator: Parser[UnaryOperator] = {
    not
  }

  def not: Parser[UnaryOperator] = {
    "~" ^^ { _ => Not() }
  }

  def rule: Parser[Rule] = {
    assumption | modusPonens
  }

  def assumption: Parser[Rule] = {
    "AS" ^^ { _ => Assumption() }
  }

  def modusPonens: Parser[Rule] = {
    "MP " ~ lineNumber ~ ", " ~ lineNumber ^^ {
      case _ ~ ln1 ~ _ ~ ln2 => ModusPonens(ln1, ln2)
    }
  }
}
