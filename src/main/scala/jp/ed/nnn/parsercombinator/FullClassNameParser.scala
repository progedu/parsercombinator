package jp.ed.nnn.parsercombinator

case class FullClassName(grade: String, className: String)

object FullClassNameParser extends MyFirstCombinator{

  def grade: Parser[String] = oneOf('1' to '3')
  def className: Parser[String] = oneOf('A' to 'D')

  def parseClassName: Parser[String] = map(combine(className, s("組")), {
    t: (String, String) => t._1
  })

  def apply(input: String): ParseResult[FullClassName] = map(combine(combine(grade, s("年")), parseClassName), {
    t: ((String, String),String) => FullClassName(t._1._1,t._2)
  })(input)
}
