package jp.ed.nnn.parsercombinator

case class NaturalNumberArith(num: Int, terms: List[(String, Int)])

object NaturalNumberArithParser extends MyFirstCombinator {

  def digitExcludingZero: Parser[String] =oneOf('1' to '9')

  def digit: Parser[String] = select(s("0"), digitExcludingZero)

  def naturalNumber: Parser[Int] = map(combine(digitExcludingZero, rep(digitExcludingZero)), {
    t: (String, List[String]) => (t._1 + t._2.mkString).toInt
  })

  def terms: Parser[(String, Int)] = combine(oneOf(Seq('+', '-')), naturalNumber)

  def apply(input: String): ParseResult[NaturalNumberArith] = map(combine(naturalNumber, rep(terms)), {
    t: (Int, List[(String, Int)]) => NaturalNumberArith(t._1, t._2)
  })(input)
}

