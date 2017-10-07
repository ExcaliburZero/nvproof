import org.scalatest.{Assertion, FlatSpec, Matchers}

import nvproof._

import scala.util.parsing.combinator._

class ValidationTests extends FlatSpec with Matchers {
  "Validation" should "pass a valid proof" in {
    val input = "1) (P ⇒ Q) AS\n" +
                "2) P AS\n" + 
                "3) Q MP 1, 2\n"
    val expected = None

    val proof = Parsers.parse(Parsers.proof, input).get
    val output = Validation.validate(proof)

    output shouldBe expected
  }

  it should "fail a invalid proof" in {
    val input = "1) (P ⇒ Q) AS\n" +
                "2) Q AS\n" + 
                "3) P MP 1, 2\n"
    val expected = Some(List(ErrorMessage("Invalid MP on line 3: first operand on line 1 (P) != line 2 (Q)")))

    val proof = Parsers.parse(Parsers.proof, input).get
    val output = Validation.validate(proof)

    output shouldBe expected
  }
}
