package pars

import pars.parser._
import pars.samples._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SampleParsersTest extends AnyFlatSpec with Matchers {
  def expect(code: String, parser: Pars.Parser[String], value: String, unconsumedPart: String) = {
    val result = parser(code)

    result.get.value shouldBe value
    result.get.unconsumedPart shouldBe unconsumedPart
  }

  "functionKeywordParser" should "parse the 'function' keyword" in {
    val code = "functionblah(){}"
    expect(code, SampleParsers.functionKeywordParser, "function", "blah(){}")
  }

  "leftBraceParser" should "parse the '(' token" in {
    val code = "(){}"
    expect(code, SampleParsers.leftBraceParser, "(", "){}")
  }
  "rightBraceParser" should "parse the ')' token" in {
    val code = "){}"
    expect(code, SampleParsers.rightBraceParser, ")", "{}")
  }
  "leftCurlyBracketParser" should "parse the '{' token" in {
    val code = "{}"
    expect(code, SampleParsers.leftCurlyBracketParser, "{", "}")
  }
  "rightCurlyBracketParser" should "parse the '}' token" in {
    val code = "}aaaaa"
    expect(code, SampleParsers.rightCurlyBracketParser, "}", "aaaaa")
  }

  "parseLeftBraceOrBracket" should "parse the '{' token" in {
    val code = "{aaaaa"
    expect(code, SampleParsers.parseLeftBraceOrBracket, "{", "aaaaa")
  }
  it should "parse the '(' token" in {
    val code = "(aaaaa"
    expect(code, SampleParsers.parseLeftBraceOrBracket, "(", "aaaaa")
  }
  it should "parse just one token" in {
    val code = "({(aaaaa"
    expect(code, SampleParsers.parseLeftBraceOrBracket, "(", "{(aaaaa")
  }

  "parseBraces" should "parse one space token" in {
    val code = "() aaaaa"

    val result = SampleParsers.parseBraces(code)

    result.get.value shouldBe Tuple2("(", ")")
    result.get.unconsumedPart shouldBe " aaaaa"
  }

  "parseSpaces" should "parse one space token" in {
    val code = " aaaaa"

    val result = SampleParsers.parseSpaces(code)

    result.get.value shouldBe List(" ")
    result.get.unconsumedPart shouldBe "aaaaa"
  }
  it should "parse more than one space tokens" in {
    val code = "    aaaaa "

    val result = SampleParsers.parseSpaces(code)

    result.get.value shouldBe List.tabulate(4)(_ => " ")
    result.get.unconsumedPart shouldBe "aaaaa "
  }
  it should "return None for no occurances of the token" in {
    val code = "aaaaa "
    val result = SampleParsers.parseSpaces(code)
    result shouldBe None
  }

  "functionParser" should "parse a function declaration" in {
    val code = "function someIdentifier{}aaaa"
    val result = SampleParsers.functionParser(code)

    // This is why we need flatten/ flatMap
    // So we'd get either a list of tokens
    val expectedResult = Tuple2(Tuple2("function", List(" ")), "someIdentifier")

    result.get.value shouldBe expectedResult
    result.get.unconsumedPart shouldBe "{}aaaa"
  }
}
