package pars.samples

import scala.util.matching.Regex

import pars.parser._
import scala.collection.mutable.AnyRefMap

object SampleParsers {
  def specificStringParser(str: String): Pars.Parser[String] = {
    (input: String) =>
      {
        val pattern = str.r
        pattern.findPrefixMatchOf(input).flatMap(result =>
          Some(Pars.ParseResult(result.toString(), input.stripPrefix(result.toString())))
        )
      }
  }
  def patternParser(pattern: Regex): Pars.Parser[String] = {
    (input: String) =>
      {
        pattern.findPrefixMatchOf(input).flatMap(result =>
          Some(Pars.ParseResult(result.toString(), input.stripPrefix(result.toString())))
        )
      }
  }

  def functionKeywordParser = specificStringParser("function")
  def leftBraceParser = specificStringParser("\\(")
  def rightBraceParser = specificStringParser("\\)")
  def leftCurlyBracketParser = specificStringParser("\\{")
  def rightCurlyBracketParser = specificStringParser("\\}")

  def singleWhiteSpacesParser = patternParser(raw"\s".r)

  def identifierParser = patternParser(raw"[a-zA-Z]+[a-zA-Z0-9]*".r)  // TODO: exclude keywords

  def parseLeftBraceOrBracket = leftBraceParser <|> leftCurlyBracketParser  // to test <|>
  def parseBraces = leftBraceParser ~ rightBraceParser  // to test ~
  def parseSpaces = singleWhiteSpacesParser.* // to test *

  // TODO: fix flatMap? There should be a much better and neater way to implement this:
  def functionParser = {
    def stringsTupleToString(t: Tuple2[String, String]): Pars.Parser[List[String]] = Pars.Parser[List[String]] {
      (input: String) =>
        {
          val parserForFirst = specificStringParser(t._1)
          val parserForSecond = specificStringParser(t._2)

          parserForFirst(input).flatMap(first =>
            parserForSecond(first.unconsumedPart).flatMap(second =>
              Some(Pars.ParseResult(List(first.value, second.value), second.unconsumedPart))
            )
          )
        }
    }

    val resultTuple = functionKeywordParser ~ parseSpaces ~ identifierParser
    // resultTuple.flatMap(stringsTupleToString)
    resultTuple
  }
}
