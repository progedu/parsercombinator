package jp.ed.nnn.parsercombinator

case class NaturalNumberArith(num: Int, terms: List[(String, Int)])

object NaturalNumberArithParser extends MyFirstCombinator {

  def digitExcludingZero: Parser[String] = oneOf('1' to '9')

  def digit: Parser[String] = oneOf(Seq('0')) match {
    case zero: Parser[String] => zero
    case _ => digitExcludingZero
  }

  def naturalNumber: Parser[Int] = map(combine(digitExcludingZero, rep(digit)), {
    t: (String, List[String]) => (t._1 + t._2.mkString).toInt
  })

  def apply(input: String): ParseResult[NaturalNumberArith] = map(
    combine(naturalNumber, rep(combine(select(oneOf(Seq('+')), oneOf(Seq('-'))), naturalNumber))), {
      t: (Int, List[(String, Int)]) => NaturalNumberArith(t._1, t._2)
    })(input)

}
