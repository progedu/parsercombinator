package jp.ed.nnn.parsercombinator

case class NaturalNumberArith(num: Int, terms: List[(String, Int)])

object NaturalNumberArithParser extends MyFirstCombinator {

  def digitExcludingZero: Parser[String] = oneOf('1' to '9')

  def digit: Parser[String] = select(s("0"), digitExcludingZero)

  def naturalNumber: Parser[Int] = map[(String,List[String]), Int](combine(digitExcludingZero, rep(digit)), result => (result._1 +: result._2).mkString.toInt)

  def expr: Parser[(Int, List[(String, Int)])] = combine(naturalNumber, rep(select(combine(s("+"), naturalNumber), combine(s("-"), naturalNumber))))

  def apply(input: String): ParseResult[NaturalNumberArith] = map[(Int, List[(String, Int)]), NaturalNumberArith](
    expr, result => NaturalNumberArith(result._1, result._2)
  )(input)
}