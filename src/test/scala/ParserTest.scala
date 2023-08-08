package pars

import pars.parser._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
// import cats.instances.function

class ParseResultTest extends AnyFlatSpec with Matchers {
  "map" should "apply a function to a parsed value and return a new ParseResult" in {
    val result = new Pars.ParseResult("abra kadabra", "some other text").map((s: String) => {
      s.toUpperCase()
    })
    result.value shouldBe "ABRA KADABRA"
    result.unconsumedPart shouldBe "some other text"
  }

  "flatMap" should "apply a function to a parsed value and return the resulting ParseResult" in {
    val result =
      new Pars.ParseResult("abra kadabra", "some other text").flatMap(
        (s: String) => {
          new Pars.ParseResult(s.toUpperCase())
        }
      )
    result.value shouldBe "ABRA KADABRA"
    result.unconsumedPart shouldBe "some other text"
  }
}
