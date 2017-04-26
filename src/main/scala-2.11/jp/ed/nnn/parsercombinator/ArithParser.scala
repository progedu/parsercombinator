package jp.ed.nnn.parsercombinator

object ArithParser extends Combinator {

  def expr: Parser[(Any, List[(String, Any)])] =
    term ~ rep((s("+") ~ term) | (s("-") ~ term))

  def term: Parser[(Any, List[(String, Any)])] =
    factor ~ rep((s("*") ~ factor) | (s("/") ~ factor))

  def factor: Parser[Any] = (floatingPointNumber ^^ {
    _.toDouble
  }) |
    s("(") ~ expr ~ s(")")

  def apply(input: String): Any = expr(input)

}
