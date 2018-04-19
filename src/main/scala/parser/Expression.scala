package parser

import lexer._
import ProgramParser._
import parser.Expression.BoolOperations.partialBasicBoolOperation

import scala.util.parsing.combinator.Parsers

object Expression {
  def expression: Parser[Expression] = {
    import NumOperations._
    import BoolOperations._
    import StringOperations._

    (
      (value <~ SEMICOLON)
      | (basicNumOperation <~ SEMICOLON)
      | (basicNumOperation.flatMap(e => partialBasicNumOperation(e)) <~ SEMICOLON)
      | (basicBoolOperation <~ SEMICOLON)
      | (basicBoolOperation.flatMap(e => partialBasicBoolOperation(e)) <~ SEMICOLON)
      | (basicStringOperation <~ SEMICOLON)
      | (basicStringOperation.flatMap(e => partialStringOperation(e)) <~ SEMICOLON)
    )
  }

  def value = number | bool | string

  object NumOperations {
    def basicNumOperation[A]: Parser[NumOperation] = addition | multiplication | division | subtraction

    def partialBasicNumOperation(leftArg: NumExpression): Parser[NumOperation] = {
      (addition(leftArg) | multiplication(leftArg) | division(leftArg) | subtraction(leftArg)).flatMap(
        operation => partialBasicNumOperation(operation) | success(operation)
      )
    }

    def basicOperationWith2Nums(op: NUM_OPERATOR): Parser[NumOperation] = {
      ((number ~ op ~ number) | (numExpr ~ op ~ numExpr)
        | (number ~ op ~ numExpr) | (numExpr ~ op ~ number)) ^^ {
        case ((n1 @ _) ~ _ ~ (n2 @ _)) => NumOperation(op, n1, n2)
      }
    }

    def basicOperationWith1Num(op: NUM_OPERATOR)(f: (NUM_OPERATOR, NumExpression) => NumOperation): Parser[NumOperation] = {
      ((op ~ number) | (op ~ numExpr)) ^^ {
        case (_ ~ n2) => f(op, n2)
      }
    }

    def leftArgumentKnown(op: NUM_OPERATOR)(arg: NumExpression): Parser[NumOperation] = {
      basicOperationWith1Num(op)(NumOperation(_ , arg, _))
    }

    def addition: Parser[NumOperation] = basicOperationWith2Nums(ADD)
    def addition(e: NumExpression) = leftArgumentKnown(ADD)(e)

    def multiplication: Parser[NumOperation] = basicOperationWith2Nums(MULTIPLY)
    def multiplication(e: NumExpression) = leftArgumentKnown(MULTIPLY)(e)

    def division: Parser[NumOperation] = basicOperationWith2Nums(DIVIDE)
    def division(e: NumExpression) = leftArgumentKnown(DIVIDE)(e)

    def subtraction: Parser[NumOperation] = basicOperationWith2Nums(SUBTRACT)
    def subtraction(e: NumExpression) = leftArgumentKnown(SUBTRACT)(e)
  }

  object BoolOperations {
    def basicBoolOperation[A]: Parser[BoolOperation] = or | greater | lower | and | equals | notEqual

    def partialBasicBoolOperation(leftArg: BoolExpression): Parser[BoolOperation] = {
      (or(leftArg) | and(leftArg) | equals(leftArg) | notEqual(leftArg) | lower(leftArg) | greater(leftArg)).flatMap(
        operation => partialBasicBoolOperation(operation) | success(operation)
      )
    }

    def basicOperationWith2Bools(op: BOOL_OPERATOR): Parser[BoolOperation] = {
      ((bool ~ op ~ bool) | (boolExpr ~ op ~ boolExpr)
        | (bool ~ op ~ boolExpr) | (boolExpr ~ op ~ bool)) ^^ {
        case ((n1 @ _) ~ _ ~ (n2 @ _)) => BoolOperation(op, n1, n2)
      }
    }

    def basicOperationWith1Bool(op: BOOL_OPERATOR)(f: (BOOL_OPERATOR, BoolExpression) => BoolOperation): Parser[BoolOperation] = {
      ((op ~ bool) | (op ~ boolExpr)) ^^ {
        case (_ ~ n2) => f(op, n2)
      }
    }

    def leftArgumentKnown(op: BOOL_OPERATOR)(arg: BoolExpression): Parser[BoolOperation] = {
      basicOperationWith1Bool(op)(BoolOperation(_ , arg, _))
    }

    def or = basicOperationWith2Bools(LOGICAL_OR)
    def or(e: BoolExpression) = leftArgumentKnown(LOGICAL_OR)(e)

    def and = basicOperationWith2Bools(LOGICAL_AND)
    def and(e: BoolExpression) = leftArgumentKnown(LOGICAL_AND)(e)

    def equals = basicOperationWith2Bools(EQUALS)
    def equals(e: BoolExpression) = leftArgumentKnown(EQUALS)(e)

    def notEqual = basicOperationWith2Bools(NOT_EQUAL)
    def notEqual(e: BoolExpression) = leftArgumentKnown(NOT_EQUAL)(e)

    def greater = basicOperationWith2Bools(MORE_THAN)
    def greater(e: BoolExpression) = leftArgumentKnown(MORE_THAN)(e)

    def lower = basicOperationWith2Bools(LESS_THAN)
    def lower(e: BoolExpression) = leftArgumentKnown(LESS_THAN)(e)
  }

  object StringOperations {
    def basicStringOperation[A]: Parser[StringOperation] = append

    def partialStringOperation(leftArg: StringExpression): Parser[StringOperation] = append(leftArg).flatMap(
      operation => partialStringOperation(operation) | success(operation)
    )

    def basicOperationWith2Strings(op: STRING_OPERATOR): Parser[StringOperation] = {
      ((string ~ op ~ string) | (stringExpr ~ op ~ stringExpr)
        | (string ~ op ~ stringExpr) | (stringExpr ~ op ~ string)) ^^ {
        case ((n1 @ _) ~ _ ~ (n2 @ _)) => StringOperation(op, n1, n2)
      }
    }

    def basicOperationWith1String(op: STRING_OPERATOR)(f: (STRING_OPERATOR, StringExpression) => StringOperation): Parser[StringOperation] = {
      ((op ~ string) | (op ~ stringExpr)) ^^ {
        case (_ ~ n2) => f(op, n2)
      }
    }

    def leftArgumentKnown(op: STRING_OPERATOR)(arg: StringExpression): Parser[StringOperation] = {
      basicOperationWith1String(op)(StringOperation(_ , arg, _))
    }

    def append = basicOperationWith2Strings(ADD)
    def append(e: StringExpression) = leftArgumentKnown(ADD)(e)
  }

  private def number: Parser[NUMBER] = accept("number", { case id @ NUMBER(_) => id })
  private def string: Parser[STRING] = accept("string", { case id @ STRING(_) => id })
  private def bool: Parser[BOOL] = accept("bool", { case id @ BOOL(_) => id })
  private def numExpr: Parser[NumOperation] = accept("num expression", { case id @ NumOperation(_, _, _) => id })
  private def stringExpr: Parser[StringOperation] = accept("string expression", { case id @ StringOperation(_, _, _) => id })
  private def boolExpr: Parser[BoolOperation] = accept("bool expression", { case id @ BoolOperation(_, _, _) => id })
}
