package lexer

import parser.AST
import scala.util.parsing.input.Positional

trait Token extends Positional with Product with Serializable
trait Expression[+A] extends Token with AST
trait NumExpression extends Expression[Int]
trait BoolExpression extends Expression[Boolean]
trait StringExpression extends Expression[String]

case class VAR_TYPE(value: String) extends Token
case class IDENTIFIER(name: String) extends Token
case class NUMBER(value: Int) extends NumExpression
case class STRING(value: String) extends StringExpression
case class BOOL(value: Boolean) extends BoolExpression
case class CONDITIONAL(value: String) extends Token

case object ASSIGNMENT extends Token
case object IF extends Token
case object ELSE extends Token
case object OPEN_BLOCK extends Token
case object CLOSE_BLOCK extends Token
case object WHILE extends Token

case object READ_INPUT extends Token
case object WRITE_OUTPUT extends Token

case object SEMICOLON extends Token
case object LEFT_PARENTHESIS extends Token
case object RIGHT_PARENTHESIS extends Token

// operators
trait OPERATOR extends Token

trait NUM_OPERATOR extends OPERATOR
trait STRING_OPERATOR extends OPERATOR
trait BOOL_OPERATOR extends OPERATOR

case object ADD extends NUM_OPERATOR with STRING_OPERATOR
case object MULTIPLY extends NUM_OPERATOR
case object DIVIDE extends NUM_OPERATOR
case object SUBTRACT extends NUM_OPERATOR

case object MORE_THAN extends BOOL_OPERATOR
case object LESS_THAN extends BOOL_OPERATOR
case object LOGICAL_OR extends BOOL_OPERATOR
case object LOGICAL_AND extends BOOL_OPERATOR
case object EQUALS extends BOOL_OPERATOR
case object NOT_EQUAL extends BOOL_OPERATOR


