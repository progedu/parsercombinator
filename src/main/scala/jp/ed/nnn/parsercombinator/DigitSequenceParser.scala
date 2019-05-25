package jp.ed.nnn.parsercombinator

case class DigitSequence(seq: Seq[String])

object DigitSequenceParser extends MyFirstCombinator {

  def digit: Parser[String] = oneOf('0' to '9')

  def digitSequence: Parser[List[String]] = rep(digit)

  def apply(input: String): ParseResult[DigitSequence] = map(digitSequence, result => DigitSequence(result))(input)

}
