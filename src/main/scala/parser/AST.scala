package parser

import lexer._
import scala.util.parsing.input.Positional

trait AST extends Positional with Product with Serializable

case class NumberAssignment(name: String, value: Int) extends AST
case class StringAssignment(name: String, value: String) extends AST
case class BoolAssignment(name: String, value: Boolean) extends AST
case class Express(name: String, value: Boolean) extends AST

case class BasicNumOperation(op: NUM_OPERAND, left: NumExpression, right: NumExpression) extends NumExpression
case class BasicStringOperation(op: STRING_OPERAND, left: StringExpression, right: StringExpression) extends StringExpression
case class BasicBoolOperation(op: BOOL_OPERAND, left: BoolExpression, right: BoolExpression) extends BoolExpression

case class AndThen(step1: AST, step2: AST) extends AST
