package jp.ed.nnn.parsercombinator

case class DigitSequence(seq: Seq[String])

object DigitSequenceParser extends MyFirstCombinator{

  def digit: Parser[String] = oneOf('0' to '9')

  def digitSeq: Parser[List[String]] = rep(digit)

  def apply(input: String): DigitSequence = {

    digitSeq(input) match {
      case Success(value, _) => DigitSequence(value)
      case _ => DigitSequence(List())
    }
  }
}
