package parser

import lexer._
import scala.util.parsing.input.Positional

trait AST extends Positional with Product with Serializable

case class NumberAssignment(name: String, value: Int) extends AST
case class StringAssignment(name: String, value: String) extends AST
case class BoolAssignment(name: String, value: Boolean) extends AST
case class Express(name: String, value: Boolean) extends AST

case class NumOperation(op: NUM_OPERATOR, left: NumExpression, right: NumExpression) extends NumExpression
case class StringOperation(op: STRING_OPERATOR, left: StringExpression, right: StringExpression) extends StringExpression
case class BoolOperation(op: BOOL_OPERATOR, left: BoolExpression, right: BoolExpression) extends BoolExpression

case class AndThen(step1: AST, step2: AST) extends AST
