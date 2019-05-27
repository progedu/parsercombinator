package jp.ed.nnn.parsercombinator

object JSONParser extends Combinator {

  def obj: Parser[Map[String, Any]]  =
    ss("{") ~> repsep(member, ss(",")) <~ ss("}") ^^ { Map() ++ _ }

  def arr: Parser[List[Any]] =
    ss("[") ~> repsep(value, ss(",")) <~ ss("]")

  def member: Parser[(String, Any)] =
    stringLiteral ~ ss(":") ~ value ^^ { t => (t._1._1, t._2) }

  def value: Parser[Any] =
    obj |
      arr |
      stringLiteral |
      (floatingPointNumber ^^ { _.toDouble }) |
      ss("null") ^^  { _ => null } |
      ss("true") ^^  { _ => true } |
      ss("false") ^^  { _ => false }

  def apply(input: String): Any = value(input)

}
