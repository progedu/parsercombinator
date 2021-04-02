package jp.ed.nnn.parsercombinator

case class FullClassName(grade: String, className: String)

object FullClassNameParser extends MyFirstCombinator {

  def grade: Parser[String] = oneOf('0' to '3')

  def class_name: Parser[String] = oneOf('A' to 'D')

  def apply(input: String): ParseResult[FullClassName] = map(combine(combine(combine(grade, s("年")),
    class_name), s("組")), {t: (((String, String), String), String) => FullClassName(t._1._1._1, t._1._2)
  })(input)

}