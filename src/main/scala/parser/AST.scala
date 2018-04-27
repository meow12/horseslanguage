package parser

import lexer._
import scala.util.parsing.input.Positional

trait AST extends Positional with Product with Serializable

trait Statement extends AST
case object EmptyAST extends AST

case class NewAssignment(varType: VAR_TYPE, identifier: IDENTIFIER, value: Expression) extends Statement
case class ExistingAssignment(identifier: IDENTIFIER, value: Expression) extends Statement
case class Operation(op: OPERATOR, left: Expression, right: Expression) extends Expression
case class IfConditional(condition: Expression, left: AST, right: AST) extends Statement
case class WhileLoop(condition: Expression, block: AST) extends Statement
case class PrintLine(text: Expression) extends Statement
case class ReadLine(variable: IDENTIFIER) extends Statement
case class AndThen(step1: AST, step2: AST) extends AST
case class Function(name: IDENTIFIER, args: List[FuncArg], body: AST) extends AST
case class FuncArg(varType: VAR_TYPE, identifier: IDENTIFIER) extends AST