package jp.ed.nnn.parsercombinator

case class FullClassName(grade: String, className: String)

object FullClassNameParser extends MyFirstCombinator{

  // 1年A組

  def grade: Parser[String] = oneOf('0' to '3')   // Success("1", "年A組")
  def className: Parser[String] = oneOf('A' to 'D')  // Success("A", "組")

  def apply(input: String): ParseResult[FullClassName] =
    map(combine(combine(combine(grade, s("年")), className), s("組")),{
      t: (((String, String), String), String) => FullClassName(t._1._1._1, t._1._2)
    })(input)

}
