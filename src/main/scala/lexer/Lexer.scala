package lexer

import entry.CompilationError.{LexerError, Location}

import scala.util.parsing.combinator.RegexParsers

object Lexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[\\s]+".r
  val numberRegex = "(:zero|:one|:two|:three|:four|:five|:six|:seven|:eight|:nine)+"

  def apply(code: String): Either[LexerError, List[Token]] = {
    parseAll(tokens, code) match {
      case NoSuccess(msg, next) => Left(LexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) => Right(result)
    }
  }

  def tokens: Parser[List[Token]] = {
    rep1(reserved | varType | semicolon | assignment | string |
      bool | number | identifier | block | parens | operator | comma | fail) // gaidiskas error handlinimas
  }

  // Tokens
  /*val semicolon: Parser[String] = ";"
  val leftParenthesis: Parser[String] = "("
  val rightParenthesis: Parser[String] = ")"
//    val blockStart: Parser[String] = "->"
//    val blockEnd: Parser[String] = "<-"

  def boolOperation = "OR" | "AND" | "EQUALS" | "NOT_EQUALS" | "MORE_THAN" | "LESS_THAN"

  def block: Parser[List[Statement]] = blockStart ~> rep1(statement) <~ blockEnd ^^ { s => s }

  def statement: Parser[Statement] = variableAssignment | ifStatement

  def identifier: Parser[IDENTIFIER] = "([a-zA-Z])([a-zA-Z0-9])*".r ^^ { str => IDENTIFIER(str) }

  def number: Parser[NUMBER] = numberRegex.r ^^ { stringNumber => NUMBER(parseDigitToNumber(stringNumber)) }

  def string: Parser[STRING] = "\"([^\"]*)\"".r ^^ { str => STRING(str.stripPrefix("\"").stripSuffix("\"")) }

  def bool: Parser[BOOL] = "(TRUE|FALSE)".r ^^ {
      case "TRUE" => BOOL(true)
      case "FALSE" => BOOL(false)
  }

  def varType: Parser[String] = "VAR"

  def varValue[A]: Parser[Value] = bool | number | string

  def variableAssignment: Parser[VARIABLE] = varType ~ identifier ~ "=" ~ varValue <~ semicolon ^^ {
      case a ~ "=" ~ b => VARIABLE(a._2, b)
  }

  /*def basicExpression: Parser[BASIC_EXPRESSION] = {
      expression ~ operand ~ expression ^^ { case (e1, op, e2) => BASIC_EXPRESSION(op, e1, e2)}
  }*/

  def condition: Parser[CONDITION] = expression ~ boolOperation ~ expression ^^ {
      case e1 ~ op ~ e2 => CONDITION(op, e1, e2)
  }

  def ifStatement: Parser[IF] = {
      "IF" ~ leftParenthesis ~ condition ~ rightParenthesis ~ "THEN" ~ block ~ opt("ELSE" ~ block) ^^ {
          case _ ~ _ ~ cond ~ _ ~ _ ~ thenBlock ~ elseOpt => {
              val elseBlock = elseOpt match { case Some(_ ~ list) => list; case _ => List.empty }
              IF(cond, thenBlock, elseBlock)
          }
      }
  }

  def expression: Parser[BASIC_EXPRESSION] = (string | number) ~ opt(operand ~ (string | number)) ^^ {
      case left ~ opAndRight => {
          opAndRight match {
              case Some(op ~ right) => BASIC_EXPRESSION(OPERAND(op.charAt(0)), left, right)
              case None => BASIC_EXPRESSION(OPERAND(" ".charAt(0)), left, null)
          }
      }
  }*/

  def identifier: Parser[IDENTIFIER] = positioned { "([a-zA-Z])([a-zA-Z0-9])*".r ^^ { str => IDENTIFIER(str) }}

  def number: Parser[NUMBER] = positioned { numberRegex.r ^^ { stringNumber => NUMBER(parseDigitToNumber(stringNumber)) }}

  def string: Parser[STRING] = positioned { "\"([^\"]*)\"".r ^^ { str => STRING(str.stripPrefix("\"").stripSuffix("\"")) }}

  def bool: Parser[BOOL] = positioned { "(TRUE|FALSE)".r ^^ {
    case "TRUE" => BOOL(true)
    case "FALSE" => BOOL(false)
  }}

  def comma = positioned { ",".r.map(_ => COMMA) }

  def fail = positioned(failure("illegal token in input"))

  def varType: Parser[VAR_TYPE] = positioned { "(BOOL|STR|INT)".r ^^ { declaration => VAR_TYPE(declaration) }}

  val assignment: Parser[ASSIGNMENT.type] = positioned { "=" ^^ { _ => ASSIGNMENT }}
  val semicolon: Parser[SEMICOLON.type] = positioned { ";" ^^ { _ => SEMICOLON }}
  val leftParenthesis: Parser[LEFT_PARENTHESIS.type ] = positioned { "(" ^^ (_ => LEFT_PARENTHESIS)}
  val rightParenthesis: Parser[RIGHT_PARENTHESIS.type ] = positioned { ")" ^^ (_ => RIGHT_PARENTHESIS) }

  def parens = positioned { leftParenthesis | rightParenthesis }

  def operator = positioned {
    ("+" | "-" | "/" | "*" | ">" | "<" | "||" | "&&" | "==" | "!=").map {
      case "+" => ADD
      case "-" => SUBTRACT
      case "*" => MULTIPLY
      case "/" => DIVIDE
      case ">" => MORE_THAN
      case "<" => LESS_THAN
      case "||" => LOGICAL_OR
      case "&&" => LOGICAL_AND
      case "==" => EQUALS
      case "!=" => NOT_EQUAL
  }}

  def reserved = positioned {
    ("IF" | "THEN" | "ELSE" | "WHILE" | "PRINT" | "READ" | "RETURN" | "FUN").map {
      case "IF" => IF
      case "THEN" => THEN
      case "READ" => READ
      case "PRINT" => PRINT
      case "WHILE" => WHILE
      case "ELSE" => ELSE
      case "RETURN" => RETURN
      case "FUN" => FUN
  }}

  def block = positioned {
    ("<-" | "->").map {
      case "<-" => CLOSE_BLOCK
      case "->" => OPEN_BLOCK
    }
  }

  // Helper functions
  def parseDigitToNumber(digitString: String): Int = {
      digitString.split(":").map(x => matchDigitToNumber(x)).mkString("").toInt
  }

  def matchDigitToNumber(digit: String): String = {
    digit match {
      case "zero" => "0"
      case "one" => "1"
      case "two" => "2"
      case "three" => "3"
      case "four" => "4"
      case "five" => "5"
      case "six" => "6"
      case "seven" => "7"
      case "eight" => "8"
      case "nine" => "9"
      case _ => ""
    }
  }
}
