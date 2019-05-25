package jp.ed.nnn.parsercombinator

import jp.ed.nnn.parsercombinator.NaturalNumberArithParser._
import org.scalatest.{DiagrammedAssertions, FlatSpec}

class NaturalNumberArithParserSpec extends FlatSpec with DiagrammedAssertions {

  "it" should "+,-を含む算術式をパース可能" in {
    assert(NaturalNumberArithParser("74+12+362-94") == Success(NaturalNumberArith(74, List("+" -> 12, "+" -> 362, "-" -> 94)), ""))
    assert(NaturalNumberArithParser("074+12+362-94") == Failure)
    assert(NaturalNumberArithParser("2525douga") == Success(NaturalNumberArith(2525, List()), "douga"))
  }

}
