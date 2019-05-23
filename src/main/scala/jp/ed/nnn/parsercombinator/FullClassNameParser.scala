package jp.ed.nnn.parsercombinator

case class FullClassName(grade: String, className: String)

object FullClassNameParser extends MyFirstCombinator {

  def grade: Parser[String] = oneOf('1' to '3')

  def className: Parser[String] = oneOf('A' to 'D')

  def apply(input: String): ParseResult[FullClassName] = map[(((String,String), String), String), FullClassName](combine(combine(combine(grade, s("年")), className), s("組")), {
    case (((grade_literal: String, _: String), classNameliteral: String), _: String) => FullClassName(grade_literal, classNameliteral)
  })(input)
}
