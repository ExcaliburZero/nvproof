package nvproof

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.token.Tokens

object Parsers extends RegexParsers {
  override val skipWhitespace = false
  private val eol = sys.props("line.separator")

  def proof: Parser[AST.Proof] = {
    rep(step <~ eol)
  }

  def step: Parser[Step] = {
    lineNumber ~ ") " ~ statement ~ " " ~ rule ^^ {
      case ln ~ _ ~ st ~ _ ~ rl => Step(ln, st, rl)
    }
  }

  def lineNumber: Parser[AST.LineNumber] = {
    "[0-9]+".r ^^ { str => str.toInt }
  }

  def statement: Parser[Statement] = {
    binaryExpression | unaryExpression | symbol
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

  def binaryExpression: Parser[Statement] = {
    "(" ~ statement ~ " " ~ binaryOperator ~ " " ~ statement ~ ")" ^^ {
      case _ ~ st1 ~ _ ~ op ~ _ ~ st2 ~ _ => BinaryExpression(st1, op, st2)
    }
  }

  def binaryOperator: Parser[BinaryOperator] = {
    implication
  }

  def implication: Parser[BinaryOperator] = {
    "⇒" ^^ { _ => Implication() }
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
