package parser

import lexer._
import Parser._

object Expression {
  def expression: Parser[Expression] =
    (exprValue <~ SEMICOLON) | (exprValue.flatMap(e => partialOperation(e)) <~ SEMICOLON)

  def conditionalExpr: Parser[Expression] =
    exprValue.flatMap(e => partialOperation(e)) | exprValue

  def exprValue = value | identifier

  def partialOperation(leftArg: Expression): Parser[Operation] = leftArgumentKnown(leftArg).flatMap(
    operation => partialOperation(operation) | success(operation)
  )

  def basicOp1(f: (OPERATOR, Expression) => Operation): Parser[Operation] = {
    ((anyOperator ~ exprValue) | (anyOperator ~ exprValue)) ^^ {
      case (op ~ n2) => f(op, n2)
    }
  }

  def leftArgumentKnown(arg: Expression): Parser[Operation] = {
    basicOp1(Operation(_ , arg, _))
  }

  def anyOperator: Parser[OPERATOR] = {
    (ADD | SUBTRACT | DIVIDE | MULTIPLY | MORE_THAN | LESS_THAN | LOGICAL_OR | LOGICAL_AND | EQUALS | NOT_EQUAL).map(
      _.asInstanceOf[OPERATOR]
    )
  }
}
