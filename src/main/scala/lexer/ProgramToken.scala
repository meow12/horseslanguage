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


sealed trait ProgramToken extends Positional with Product with Serializable
trait Value extends ProgramToken

case class VAR_TYPE(value: String) extends ProgramToken
case object ASSIGNMENT extends ProgramToken
case class IDENTIFIER(name: String) extends ProgramToken
case class NUMBER(value: Int) extends Value
case class STRING(value: String) extends Value
case class BOOL(value: Boolean) extends Value

case class CONDITIONAL(value: String) extends ProgramToken

case object IF extends ProgramToken
case object ELSE extends ProgramToken
case object OPEN_BLOCK extends ProgramToken
case object CLOSE_BLOCK extends ProgramToken
case object WHILE extends ProgramToken

case object READ_INPUT extends ProgramToken
case object WRITE_OUTPUT extends ProgramToken

case object SEMICOLON extends ProgramToken
case object LEFT_PARENTHESIS extends ProgramToken
case object RIGHT_PARENTHESIS extends ProgramToken

// operands
sealed trait OPERAND extends ProgramToken
case object ADD extends OPERAND
case object MULTIPLY extends OPERAND
case object DIVIDE extends OPERAND
case object SUBTRACT extends OPERAND

case object EQUALS extends OPERAND




