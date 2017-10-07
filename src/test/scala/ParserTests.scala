import org.scalatest.{Assertion, FlatSpec, Matchers}

import nvproof._

import scala.util.parsing.combinator._

class ParsersTests extends FlatSpec with Matchers {
  "Parsers" should "parse a modus ponens rule" in {
    val input = "MP 1, 20"
    val expected = ModusPonens(1, 20)

    val output = Parsers.parse(Parsers.modusPonens, input)

    output.successful shouldBe true
    output.get shouldBe expected
  }

  it should "parse a symbol" in {
    val input = "P"
    val expected = Symbol('P')

    val output = Parsers.parse(Parsers.symbol, input)

    output.successful shouldBe true
    output.get shouldBe expected
  }

  it should "parse a unary operator" in {
    val input = "~"
    val expected = Not()

    val output = Parsers.parse(Parsers.unaryOperator, input)

    output.successful shouldBe true
    output.get shouldBe expected
  }

  it should "parse a unary expression" in {
    val input = "~P"
    val expected = UnaryExpression(Not(), Symbol('P'))

    val output = Parsers.parse(Parsers.unaryExpression, input)

    output.successful shouldBe true
    output.get shouldBe expected
  }

  it should "parse a binary expression" in {
    val input = "(P ⇒ Q)"
    val expected = BinaryExpression(Symbol('P'), Implication(), Symbol('Q'))

    val output = Parsers.parse(Parsers.binaryExpression, input)

    output.successful shouldBe true
    output.get shouldBe expected
  }

  it should "parse a step" in {
    val input = "10) ~P MP 1, 2"
    val expected = Step(10, UnaryExpression(Not(), Symbol('P')), ModusPonens(1, 2))

    val output = Parsers.parse(Parsers.step, input)

    output.successful shouldBe true
    output.get shouldBe expected
  }

  it should "parse a step with a binary expression" in {
    val input = "10) (~Q ⇒ P) AS"
    val expected = Step(10, BinaryExpression(UnaryExpression(Not(), Symbol('Q')), Implication(), Symbol('P')), Assumption())

    val output = Parsers.parse(Parsers.step, input)

    output.successful shouldBe true
    output.get shouldBe expected
  }

  it should "parse a proof" in {
    val input = "1) (P ⇒ Q) AS\n" +
                "2) P AS\n" + 
                "3) Q MP 1, 2\n"
    val expected = List(
        Step(1, BinaryExpression(Symbol('P'), Implication(), Symbol('Q')), Assumption())
      , Step(2, Symbol('P'), Assumption())
      , Step(3, Symbol('Q'), ModusPonens(1, 2))
    )

    val output = Parsers.parse(Parsers.proof, input)

    output.successful shouldBe true
    output.get shouldBe expected
  }
}
