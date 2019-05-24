package jp.ed.nnn.parsercombinator

import org.scalatest._

class DigitSequenceParserSpec extends FlatSpec with DiagrammedAssertions {
  import jp.ed.nnn.parsercombinator.DigitSequenceParser._
  
  it should "連続する数値の文字列をパース可能" in {
    assert(DigitSequenceParser("192168001128") == Success(DigitSequence(List("1", "9", "2", "1", "6", "8", "0", "0", "1", "1", "2", "8")), ""))
    assert(DigitSequenceParser("346Production") == Success(DigitSequence(List("3", "4", "6")), "Production"))
    assert(DigitSequenceParser("digitSequence") == Failure)
  }
}