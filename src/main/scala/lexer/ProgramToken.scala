package lexer

import scala.util.parsing.input.Positional

/*sealed trait ProgramToken extends Positional
sealed trait Expr extends ProgramToken
sealed trait Statement extends ProgramToken
sealed trait Value extends Expr

case class BASIC_EXPRESSION(op: OPERAND, left: Expr, right: Expr) extends Expr
case class CONDITION(op: String, left: Expr, right: Expr) extends Expr
case class IDENTIFIER(name: String) extends Expr
case class NUMBER(value: Int) extends Value
case class STRING(value: String) extends Value
case class BOOL(value: Boolean) extends Value
case class OPERAND(value: Char) extends Value

case class VARIABLE(name: IDENTIFIER, value: Value) extends Statement
case class IF(condition: CONDITION, trueBlock: List[Statement], falseBlock: List[Statement]) extends Statement*/


sealed trait ProgramToken extends Positional

case class VAR_TYPE(value: String) extends ProgramToken
case object ASSIGNMENT extends ProgramToken
case class IDENTIFIER(name: String) extends ProgramToken
case class NUMBER(value: Int) extends ProgramToken
case class STRING(value: String) extends ProgramToken
case class BOOL(value: Boolean) extends ProgramToken

case class OPERATOR(value: String) extends ProgramToken
case class CONDITIONAL(value: String) extends ProgramToken

case class IF() extends ProgramToken
case class ELSE() extends ProgramToken
case object OPEN_BLOCK extends ProgramToken
case object CLOSE_BLOCK extends ProgramToken
case class WHILE() extends ProgramToken

case class READ_INPUT() extends ProgramToken
case class WRITE_OUTPUT() extends ProgramToken

case object SEMICOLON extends ProgramToken
case object LEFT_PARENTHESIS extends ProgramToken
case object RIGHT_PARENTHESIS extends ProgramToken



