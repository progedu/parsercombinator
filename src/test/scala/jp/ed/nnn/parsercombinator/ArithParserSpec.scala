package jp.ed.nnn.parsercombinator

import jp.ed.nnn.parsercombinator.ArithParser._
import org.scalatest._

class ArithParserSpec extends FlatSpec with DiagrammedAssertions {

  it should "算術式をパース可能" in {
    assert(ArithParser("(3.4+3*2.3)*4/1.3-3") == Success((((("3.4",List()),List(("+",("3",List(("*","2.3")))))),List(("*","4"), ("/","1.3"))),List(("-",("3",List())))),""))
  }
}
