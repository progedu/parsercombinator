package jp.ed.nnn.parsercombinator

// 1 + 2 - 3 + 4 ------>  NaturalNumberArith(1, List(("+", 2), ("-", 3), ("+", 4))
case class NaturalNumberArith(num: Int, terms: List[(String, Int)])

object NaturalNumberArithParser extends MyFirstCombinator{


  def digitExcZero: Parser[String] = oneOf('1' to '9')

  def naturalNumber: Parser[Int] = input => rep(digitExcZero)(input) match {
    case Success(value: List[String], next) => Success(value.foldLeft("")(_ + _).toInt, next)
    case _ => Failure
  }

  def plusParser: Parser[String] = s("+")
  def minusParser: Parser[String] = s("-")
  def plusMinusParser: Parser[String] = select(plusParser, minusParser)

  // <expr> ::= <natural-number> { "+" <natural-number> | "-" <natural-number> }
  // term は最初のnaturalNumber(input)で取得
  // 再帰で("+", natural-number) をnextがなくなるまで取得し続ける。
  // "+" と "-" は s("+"), s("-") でSucess("+", next) または Failureという形で取得する。

  def expression(numParser: Parser[Int], strParser: Parser[String]): Parser[(Int, List[(String, Int)])] = input => {

    val term0 = numParser(input)

    def expressionRec(list: List[(String, Int)], term: Int): Parser[(Int, List[(String, Int)])] = next => {

      if (next.length == 0) {
        list match {
          case List() => Failure
          case a:List[(String, Int)] => Success((term, a), "")
        }
      } else {
        strParser(next) match {
          case Success(strVal, next1) => numParser(next1) match {
            case Success(numVal, next2) => expressionRec(list :+ (strVal, numVal), term)(next2)
            case Failure => Failure
          }
          case Failure => Failure
        }
      }
    }

    term0 match {
      case Success(value, next0) => expressionRec(List(), value)(next0)
      case Failure => Failure
    }

  }

  def apply(input: String): NaturalNumberArith = {
    expression(naturalNumber, plusMinusParser)(input) match {
      case Success(value, _) => NaturalNumberArith(value._1, value._2)
      case Failure => NaturalNumberArith(0, List())
    }

  }
