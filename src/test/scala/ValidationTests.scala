import org.scalatest.{Assertion, FlatSpec, Matchers}

import nvproof._

import scala.util.parsing.combinator._

class ValidationTests extends FlatSpec with Matchers {
  "Validation" should "pass a valid proof with modus ponens" in {
    val input = "1) (P -> Q) AS\n" +
                "2) P AS\n" +
                "3) Q MP 1, 2\n"
    val expected = None

    val proof = Parsers.parse(Parsers.proof, input).get
    val output = Validation.validate(proof)

    output shouldBe expected
  }

  it should "fail a invalid proof with modus ponens" in {
    val input = "1) (P -> Q) AS\n" +
                "2) Q AS\n" +
                "3) P MP 1, 2\n"
    val expected = Some(List(ErrorMessage("Invalid MP on line 3: first operand on line 1 (P) != line 2 (Q)")))

    val proof = Parsers.parse(Parsers.proof, input).get
    val output = Validation.validate(proof)

    output shouldBe expected
  }

  it should "pass a valid proof with contraposition" in {
    val input = "1) (P -> Q) AS\n" +
                "3) (~Q -> ~P) Contra 1\n"
    val expected = None

    val proof = Parsers.parse(Parsers.proof, input).get
    val output = Validation.validate(proof)

    output shouldBe expected
  }

  it should "fail a invalid proof with contraposition" in {
    val input = "1) (P -> Q) AS\n" +
                "3) (~P -> ~Q) Contra 1\n"
    val expected = Some(List(ErrorMessage("Invalid Contra on line 3: first operand on line 1 (P) != un-negated second operand on line 3 (Q)")))

    val proof = Parsers.parse(Parsers.proof, input).get
    val output = Validation.validate(proof)

    output shouldBe expected
  }

  it should "pass a valid proof with double negation" in {
    val input = "1) ~([]P -> <>~~~Q) AS\n" +
                "2) ~~~(~~[]P -> <>~Q) DN* 1\n"
    val expected = None

    val proof = Parsers.parse(Parsers.proof, input).get
    val output = Validation.validate(proof)

    output shouldBe expected
  }

  it should "fail a invalid proof with double negation" in {
    val input = "1) (P -> Q) AS\n" +
                "3) (~P -> ~Q) DN* 1\n"
    val expected = Some(List(ErrorMessage("Invalid DN* on line 3: line 3 is not equivalent to line 1 through DN*")))

    val proof = Parsers.parse(Parsers.proof, input).get
    val output = Validation.validate(proof)

    output shouldBe expected
  }
}
