package nvproof

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.token.Tokens

object Parsers extends RegexParsers {
  override val skipWhitespace = false
  private val eol = sys.props("line.separator")
  private val eof = "\\z".r

  def proof: Parser[AST.Proof] = {
    rep(step <~ eol) <~ eof
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
    not | necessary | possible
  }

  def not: Parser[UnaryOperator] = {
    "~" ^^ { _ => Not() }
  }

  def necessary: Parser[UnaryOperator] = {
    "[]" ^^ { _ => Necessary() }
  }

  def possible: Parser[UnaryOperator] = {
    "<>" ^^ { _ => Possible() }
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
    "->" ^^ { _ => Implication() }
  }

  def rule: Parser[Rule] = {
    assumption | modusPonens | contraposition | doubleNegation | l1 | l2 | l3 | m1 | m2 | m3 | m4 | m5 | necessitation | byDefModal
  }

  def assumption: Parser[Rule] = {
    "AS" ^^ { _ => Assumption() }
  }

  def modusPonens: Parser[Rule] = {
    "MP " ~ lineNumber ~ ", " ~ lineNumber ^^ {
      case _ ~ ln1 ~ _ ~ ln2 => ModusPonens(ln1, ln2)
    }
  }

  def contraposition: Parser[Rule] = {
    "Contra " ~> lineNumber ^^ {
      case ln => Contraposition(ln)
    }
  }

  def doubleNegation: Parser[Rule] = {
    "DN* " ~ lineNumber ^^ {
      case _ ~ ln => DoubleNegation(ln)
    }
  }

  def l1: Parser[Rule] = {
    "L1" ^^ { case _ => L1() }
  }

  def l2: Parser[Rule] = {
    "L2" ^^ { case _ => L2() }
  }

  def l3: Parser[Rule] = {
    "L3" ^^ { case _ => L3() }
  }

  def m1: Parser[Rule] = {
    "M1" ^^ { case _ => M1() }
  }

  def m2: Parser[Rule] = {
    "M2" ^^ { case _ => M2() }
  }

  def m3: Parser[Rule] = {
    "M3" ^^ { case _ => M3() }
  }

  def m4: Parser[Rule] = {
    "M4" ^^ { case _ => M4() }
  }

  def m5: Parser[Rule] = {
    "M5" ^^ { case _ => M5() }
  }

  def necessitation: Parser[Rule] = {
    "Necess " ~> lineNumber ^^ {
      case ln => Necessitation(ln)
    }
  }

  def byDefModal: Parser[Rule] = {
    lineNumber <~ " by def modal" ^^ {
      case ln => ByDefModal(ln)
    }
  }
}
