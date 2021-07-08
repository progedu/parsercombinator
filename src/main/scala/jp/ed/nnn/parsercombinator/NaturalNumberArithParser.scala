package jp.ed.nnn.parsercombinator

case class NaturalNumberArith(num: Int, terms: List[(String, Int)])

object NaturalNumberArithParser extends MyFirstCombinator {
  def digitNonZero: Parser[String] = oneOf('1' to '9')

  def digit: Parser[String] = select(s("0"), digitNonZero)

  def naturalNumber: Parser[Int] = combine(digitNonZero, rep(digit)).andThen {
    case Failure => Failure
    case Success((str, list), rest) => Success((str + list.mkString).toInt, rest)
  }

  def additionTerm: Parser[(String, Int)] = combine(s("+"), naturalNumber)

  def subtractTerm: Parser[(String, Int)] = combine(s("-"), naturalNumber)

  def expr: Parser[(Int, List[(String, Int)])] = combine(naturalNumber, rep(select(additionTerm, subtractTerm)))

  def apply(input: String): ParseResult[NaturalNumberArith] = map(expr, {
    t: (Int, List[(String, Int)]) => NaturalNumberArith(t._1, t._2)
  })(input)
}
