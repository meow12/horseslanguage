package lexer

import parser.AST

import scala.util.parsing.input.Positional

trait Token extends Positional with Product with Serializable
trait Expression extends Token with AST
trait NumExpression extends Expression
trait BoolExpression extends Expression
trait StringExpression extends Expression

case class VAR_TYPE(value: String) extends Token
case class IDENTIFIER(name: String) extends Token with Expression
case class NUMBER(value: Int) extends NumExpression
case class STRING(value: String) extends StringExpression
case class BOOL(value: Boolean) extends BoolExpression

case object ASSIGNMENT extends Token
case object IF extends Token
case object THEN extends Token
case object ELSE extends Token
case object READ extends Token
case object PRINT extends Token
case object OPEN_BLOCK extends Token
case object CLOSE_BLOCK extends Token
case object WHILE extends Token
case object FUN extends Token
case object RETURN extends Token

case object SEMICOLON extends Token
case object LEFT_PARENTHESIS extends Token
case object RIGHT_PARENTHESIS extends Token
case object COMMA extends Token
case object END extends Token with AST

trait OPERATOR extends Token

trait NUM_OPERATOR extends OPERATOR
trait STRING_OPERATOR extends OPERATOR
trait BOOL_OPERATOR extends OPERATOR

case object ADD extends OPERATOR
case object MULTIPLY extends OPERATOR
case object DIVIDE extends OPERATOR
case object SUBTRACT extends OPERATOR

case object MORE_THAN extends OPERATOR
case object LESS_THAN extends OPERATOR
case object LOGICAL_OR extends OPERATOR
case object LOGICAL_AND extends OPERATOR
case object EQUALS extends OPERATOR
case object NOT_EQUAL extends OPERATOR



