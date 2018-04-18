package parser

import compiler.{Location, ParserError}
import lexer._
import parser.ProgramParser.BoolOperations.{basicOperationWith2Bools, leftArgumentKnown}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object ProgramParser extends Parsers {
    override type Elem = ProgramToken

    class ProgramTokenReader(tokens: Seq[ProgramToken]) extends Reader[ProgramToken] {
        override def first: ProgramToken = tokens.head
        override def atEnd: Boolean = tokens.isEmpty
        override def pos: Position = NoPosition
        override def rest: Reader[ProgramToken] = new ProgramTokenReader(tokens.tail)
    }

    def apply(tokens: Seq[ProgramToken]): Either[ParserError, AST] = {
        val reader = new ProgramTokenReader(tokens)
        program(reader) match {
          case NoSuccess(msg, next) => Left(ParserError(Location(next.pos.line, next.pos.column), msg))
          case Success(result, _) => Right(result)
        }
    }

    def program: Parser[AST] = positioned {
        phrase(block)
    }

    def block: Parser[AST] = positioned {
        rep1(statement) ^^ { stmtList => stmtList reduceRight AndThen }
    }

    def statement: Parser[AST] = {
        assignment
    }

    private def assignment[A]: Parser[AST] = {
        (varType ~ identifier ~ ASSIGNMENT ~ expression).flatMap {
            case varType ~ identifier ~ _ ~ value  => (varType.value, value) match {
                case ("STR", STRING(s)) => success(StringAssignment(identifier.name, s))
                case ("BOOL", BOOL(b)) => success(BoolAssignment(identifier.name, b))
                case ("INT", NUMBER(i)) => success(NumberAssignment(identifier.name, i))
                case ("INT", x @ BasicNumOperation(_ , _, _)) => success(x)
                case ("STR", x @ BasicStringOperation(_ , _, _)) => success(x)
                case ("BOOL", x @ BasicBoolOperation(_ , _, _)) => success(x)
                case _ => failure("kintamojo tipas neatitinka kintamojo reiksmes: " + varType.value + " identifier = " + value)
            }
        }
    }

    def expression = {
        import NumOperations._
        import BoolOperations._
        import StringOperations._

        ((value <~ SEMICOLON)
        | (basicNumOperation <~ SEMICOLON)
        | (basicNumOperation.flatMap(e => partialBasicNumOperation(e)) <~ SEMICOLON)
        | (basicBoolOperation <~ SEMICOLON)
        | (basicBoolOperation.flatMap(e => partialBasicBoolOperation(e)) <~ SEMICOLON)
        | (basicStringOperation <~ SEMICOLON)
        | (basicStringOperation.flatMap(e => partialStringOperation(e)) <~ SEMICOLON))
    }

    def value = number | bool | string

    object NumOperations {
        def basicNumOperation[A]: Parser[BasicNumOperation] = addition | multiplication | division | subtraction

        def partialBasicNumOperation(leftArg: NumExpression): Parser[BasicNumOperation] = {
            addition(leftArg) | multiplication(leftArg) | division(leftArg) | subtraction(leftArg)
        }

        def basicOperationWith2Nums(op: NUM_OPERAND): Parser[BasicNumOperation] = {
            ((number ~ op ~ number) | (numExpr ~ op ~ numExpr)
              | (number ~ op ~ numExpr) | (numExpr ~ op ~ number)) ^^ {
                case ((n1 @ _) ~ _ ~ (n2 @ _)) => BasicNumOperation(op, n1, n2)
            }
        }

        def basicOperationWith1Num(op: NUM_OPERAND)(f: (NUM_OPERAND, NumExpression) => BasicNumOperation): Parser[BasicNumOperation] = {
            ((op ~ number) | (op ~ numExpr)) ^^ {
                case (_ ~ n2) => f(op, n2)
            }
        }

        def leftArgumentKnown(op: NUM_OPERAND)(arg: NumExpression): Parser[BasicNumOperation] = {
            basicOperationWith1Num(op)(BasicNumOperation(_ , arg, _))
        }

        def addition: Parser[BasicNumOperation] = basicOperationWith2Nums(ADD)
        def addition(e: NumExpression) = leftArgumentKnown(ADD)(e)

        def multiplication: Parser[BasicNumOperation] = basicOperationWith2Nums(MULTIPLY)
        def multiplication(e: NumExpression) = leftArgumentKnown(MULTIPLY)(e)

        def division: Parser[BasicNumOperation] = basicOperationWith2Nums(DIVIDE)
        def division(e: NumExpression) = leftArgumentKnown(DIVIDE)(e)

        def subtraction: Parser[BasicNumOperation] = basicOperationWith2Nums(SUBTRACT)
        def subtraction(e: NumExpression) = leftArgumentKnown(SUBTRACT)(e)
    }

    object BoolOperations {
        def basicBoolOperation[A]: Parser[BasicBoolOperation] = or | greater | lower | and | equals | notEqual

        def partialBasicBoolOperation(leftArg: BoolExpression): Parser[BasicBoolOperation] = {
            or(leftArg) | and(leftArg) | equals(leftArg) | notEqual(leftArg) | lower(leftArg) | greater(leftArg)
        }

        def basicOperationWith2Bools(op: BOOL_OPERAND): Parser[BasicBoolOperation] = {
            ((bool ~ op ~ bool) | (boolExpr ~ op ~ boolExpr)
              | (bool ~ op ~ boolExpr) | (boolExpr ~ op ~ bool)) ^^ {
                case ((n1 @ _) ~ _ ~ (n2 @ _)) => BasicBoolOperation(op, n1, n2)
            }
        }

        def basicOperationWith1Bool(op: BOOL_OPERAND)(f: (BOOL_OPERAND, BoolExpression) => BasicBoolOperation): Parser[BasicBoolOperation] = {
            ((op ~ bool) | (op ~ boolExpr)) ^^ {
                case (_ ~ n2) => f(op, n2)
            }
        }

        def leftArgumentKnown(op: BOOL_OPERAND)(arg: BoolExpression): Parser[BasicBoolOperation] = {
            basicOperationWith1Bool(op)(BasicBoolOperation(_ , arg, _))
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
        def basicStringOperation[A]: Parser[BasicStringOperation] = addition

        def partialStringOperation(leftArg: StringExpression): Parser[BasicStringOperation] = addition(leftArg)

        def basicOperationWith2Strings(op: STRING_OPERAND): Parser[BasicStringOperation] = {
            ((string ~ op ~ string) | (stringExpr ~ op ~ stringExpr)
              | (string ~ op ~ stringExpr) | (stringExpr ~ op ~ string)) ^^ {
                case ((n1 @ _) ~ _ ~ (n2 @ _)) => BasicStringOperation(op, n1, n2)
            }
        }

        def basicOperationWith1String(op: STRING_OPERAND)(f: (STRING_OPERAND, StringExpression) => BasicStringOperation): Parser[BasicStringOperation] = {
            ((op ~ string) | (op ~ stringExpr)) ^^ {
                case (_ ~ n2) => f(op, n2)
            }
        }

        def leftArgumentKnown(op: STRING_OPERAND)(arg: StringExpression): Parser[BasicStringOperation] = {
            basicOperationWith1String(op)(BasicStringOperation(_ , arg, _))
        }

        def addition = basicOperationWith2Strings(ADD)
        def addition(e: StringExpression) = leftArgumentKnown(ADD)(e)
    }

    private def identifier: Parser[IDENTIFIER] = accept("identifier", { case id @ IDENTIFIER(_) => id })
    private def varType: Parser[VAR_TYPE] = accept("var_type", { case id @ VAR_TYPE(_) => id })
    private def number: Parser[NUMBER] = accept("number", { case id @ NUMBER(_) => id })
    private def string: Parser[STRING] = accept("string", { case id @ STRING(_) => id })
    private def bool: Parser[BOOL] = accept("bool", { case id @ BOOL(_) => id })
    private def numExpr: Parser[BasicNumOperation] = accept("num expression", { case id @ BasicNumOperation(_, _, _) => id })
    private def boolExpr: Parser[BasicBoolOperation] = accept("bool expression", { case id @ BasicBoolOperation(_, _, _) => id })
    private def stringExpr: Parser[BasicStringOperation] = accept("string expression", { case id @ BasicStringOperation(_, _, _) => id })

//    private def bOperation: Parser[BOOL] = accept("bool", { case id @ BOOL(_) => id })

    /*
    def block: Parser[AST] = positioned {
        rep1(statement) ^^ { case stmtList => stmtList reduceRight AndThen }
    }

    def statement: Parser[AST] = positioned {
      val exit = EXIT() ^^ (_ => Exit)
      val readInput = READINPUT() ~ rep(identifier ~ COMMA()) ~ identifier ^^ {
        case read ~ inputs ~ IDENTIFIER(lastInput) => ReadInput(inputs.map(_._1.str) ++ List(lastInput))
      }
      val callService = CALLSERVICE() ~ literal ^^ {
        case call ~ LITERAL(serviceName) => CallService(serviceName)
      }
      val switch = SWITCH() ~ COLON() ~ INDENT() ~ rep1(ifThen) ~ opt(otherwiseThen) ~ DEDENT() ^^ {
        case _ ~ _ ~ _ ~ ifs ~ otherwise ~ _ => Choice(ifs ++ otherwise)
      }
      exit | readInput | callService | switch
    }

    def ifThen: Parser[IfThen] = positioned {
      (condition ~ ARROW() ~ INDENT() ~ block ~ DEDENT()) ^^ {
        case cond ~ _ ~ _ ~ block ~ _ => IfThen(cond, block)
      }
    }

    def otherwiseThen: Parser[OtherwiseThen] = positioned {
      (OTHERWISE() ~ ARROW() ~ INDENT() ~ block ~ DEDENT()) ^^ {
        case _ ~ _ ~ _ ~ block ~ _ => OtherwiseThen(block)
      }
    }

    def condition: Parser[Equals] = positioned {
      (identifier ~ EQUALS() ~ literal) ^^ { case IDENTIFIER(id) ~ eq ~ LITERAL(lit) => Equals(id, lit) }
    }

    private def identifier: Parser[IDENTIFIER] = positioned {
      accept("identifier", { case id @ IDENTIFIER(name) => id })
    }

    private def literal: Parser[LITERAL] = positioned {
      accept("string literal", { case lit @ LITERAL(name) => lit })
    }*/

}
