package pars.parser

import scala.collection.mutable.AnyRefMap

object Pars {
  case class ParseResult[+T](val value: T, val unconsumedPart: String = "") extends AnyRef {
    def map[U](f: T => U): ParseResult[U] = ParseResult(f(value), unconsumedPart)
    def flatMap[U](f: T => ParseResult[U]): ParseResult[U] = ParseResult(f(value).value, unconsumedPart)
  }

  // type constructor
  def Parser[T](f: String => Option[ParseResult[T]]): Parser[T] =
    new Parser[T] { def apply(input: String): Option[ParseResult[T]] = f(input) }

  abstract class Parser[+T] extends AnyRef {
    def apply(input: String): Option[ParseResult[T]]

    def map[U](f: T => U): Parser[U] = Parser[U] {
      (input: String) =>
        this(input).flatMap(value => Some(value map f))
    }

    // monadic combinator / bind / flatMap ?
    // unwraps a monadic value and inserts it into a monadic function/ expression
    def flatMap[U](f: T => Parser[U]): Parser[U] = {
      (input: String) => this(input).flatMap(result => f(result.value)(result.unconsumedPart))
    }

    // parser combinators
    def <|>[U >: T](p: Parser[U]): Parser[U] = Parser[U] {
      (input: String) => this(input).orElse(p(input))
    } // match A orElse B

    def ~[U](p: Parser[U]): Parser[Tuple2[T, U]] = Parser[Tuple2[T, U]] {
      (input: String) =>
        {
          this(input) match {
            case Some(v1) => p(v1.unconsumedPart) match {
                case Some(v2) => Some(ParseResult(
                    Tuple2(
                      v1.value,
                      v2.value
                    ),
                    v2.unconsumedPart
                  ))
                case None => None
              }
            case None => None
          }
        }
    } // match A andThen B

    def *(): Parser[List[T]] = {
      def many(input: String): ParseResult[List[T]] = {
        if (input.isEmpty()) ParseResult(List(), "")

        this(input) match {
          case Some(v) => {
            val next = many(v.unconsumedPart)
            ParseResult(v.value +: next.value, next.unconsumedPart)
          }
          case None => ParseResult(List(), input)
        }
      }

      Parser[List[T]] {
        (input: String) =>
          {
            if (input.isEmpty()) None

            many(input) match {
              case ParseResult(x :: xs, unconsumedPart) => Some(ParseResult(x :: xs, unconsumedPart))
              case ParseResult(List(), _) => None
            }
          }
      }
    } // repeat, at least once
  }
}
