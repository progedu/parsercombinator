package jp.ed.nnn.parsercombinator

case class NaturalNumberArith(num: Int, terms: List[(String, Int)])

object NaturalNumberArithParser extends MyFirstCombinator {

  def digitExcludingZero: Parser[Int] = map(oneOf('1' to '9'), { digit: String => digit.toInt })

  def digit: Parser[Int] = select(digitExcludingZero, map(s("0"), {
    digit: String => digit.toInt
  }))

  def naturalNumber: Parser[Int] = map(combine(digitExcludingZero, rep(digit)), {
    t: (Int, List[Int]) => {
      val list = t._2
      list.foldLeft(t._1)((x,y) => 10*x + y)
    }
  })

  def apply(input: String): ParseResult[NaturalNumberArith] = map(combine(
    naturalNumber, rep(combine(oneOf(Seq('-', '+')), naturalNumber))), {
    t: (Int, List[(String, Int)]) => NaturalNumberArith(t._1, t._2)
  })(input)

}