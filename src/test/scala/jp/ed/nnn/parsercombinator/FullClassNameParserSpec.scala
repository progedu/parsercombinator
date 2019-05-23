package jp.ed.nnn.parsercombinator
import org.scalatest._

class FullClassNameParserSpec extends FlatSpec with DiagrammedAssertions {
  import jp.ed.nnn.parsercombinator.FullClassNameParser._
  // <grade> ::= "1" | "2" | "3"
  // <class-name> ::= "A" | "B" | "C" | "D"
  // <full-class-name> ::=  <grade> "年" <class-name> "組"
  it should "上記のBNFをパース可能である" in {
    assert(FullClassNameParser("1年A組") == Success(FullClassName("1", "A"), ""))
    assert(FullClassNameParser("3年B組金八先生") == Success(FullClassName("3", "B"), "金八先生"))
    assert(FullClassNameParser("1年は組忍たま乱太郎") == Failure)
  }
}