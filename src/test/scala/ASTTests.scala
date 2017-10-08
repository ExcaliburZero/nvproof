import org.scalatest.{Assertion, FlatSpec, Matchers}

import nvproof._

import scala.util.parsing.combinator._

class ASTTests extends FlatSpec with Matchers {
  "AST" should "allow one to represent a simple proof" in {
    List(
        Step(1, BinaryExpression(Symbol('P'), Implication(), Symbol('Q')), Assumption(), 0)
      , Step(2, UnaryExpression(Not(), Symbol('Q')), ModusPonens(1, 2), 0)
    )

    1 shouldBe 1
  }
}
