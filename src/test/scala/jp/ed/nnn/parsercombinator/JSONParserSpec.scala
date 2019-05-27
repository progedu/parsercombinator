package jp.ed.nnn.parsercombinator

import jp.ed.nnn.parsercombinator.JSONParser._
import org.scalatest._

class JSONParserSpec extends FlatSpec with DiagrammedAssertions {

  it should "JSONをパース可能" in {
    val json = "[1.0, -2.0, true, false,  \rnull,\n{\"hoge\":\"fuga\",\"piyo\":null}]"
    JSONParser(json) match {
      case Success(result, _) =>
        assert(result == List(1.0, -2.0, true, false, null, Map("hoge" -> "fuga", "piyo" -> null)))
      case _ => assert(false)
    }
  }
}
