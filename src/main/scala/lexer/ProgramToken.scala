package lexer

import parser.AST
import scala.util.parsing.input.Positional

trait ProgramToken extends Positional with Product with Serializable
trait Expression extends ProgramToken with AST
trait NumExpression extends Expression
trait BoolExpression extends Expression
trait StringExpression extends Expression

case class VAR_TYPE(value: String) extends ProgramToken
case class IDENTIFIER(name: String) extends ProgramToken
case class NUMBER(value: Int) extends NumExpression
case class STRING(value: String) extends StringExpression
case class BOOL(value: Boolean) extends BoolExpression
case class CONDITIONAL(value: String) extends ProgramToken

case object ASSIGNMENT extends ProgramToken
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

// operators
trait OPERATOR extends ProgramToken

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


