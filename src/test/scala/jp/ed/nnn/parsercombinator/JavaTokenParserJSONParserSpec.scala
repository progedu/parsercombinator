package jp.ed.nnn.parsercombinator

import jp.ed.nnn.parsercombinator.JavaTokenParsersJSONParser._
import org.scalatest._

class JavaTokenParserJSONParserSpec extends FlatSpec with DiagrammedAssertions {

  it should "JSONをパース可能" in {
    val json = "[1.0,-2.0,true,false,null,{\"hoge\":\"fuga\",\"piyo\":null}]"
    JavaTokenParsersJSONParser(json) match {
      case Success(result, _) =>
        assert(result == List(1.0, -2.0, true, false, null, Map("\"hoge\"" -> "\"fuga\"", "\"piyo\"" -> null)))
      case _ => assert(false)
    }
  }
}
