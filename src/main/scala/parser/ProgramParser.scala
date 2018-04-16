package parser

import compiler.{Location, ParserError}
import lexer._

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
                case (_, x @ BasicOperation(_, _, _)) => success(x)
                case ("STR", STRING(s)) => success(StringAssignment(identifier.name, s))
                case ("BOOL", BOOL(b)) => success(BoolAssignment(identifier.name, b))
                case ("INT", NUMBER(i)) => success(NumberAssignment(identifier.name, i))
                case _ => failure("kintamojo tipas neatitinka kintamojo reiksmes: " + varType.value + " identifier = " + value)
            }
        }
    }

    def expression = ((number | bool | string) <~ SEMICOLON) | (rep1(basicOperation) <~ SEMICOLON)

    def basicOperation[A]: Parser[BasicOperation] = addition | multiplication | division | subtraction

    def addition = {
        ((string ~ rep1(ADD ~ string)) | (number ~ rep1(ADD ~ number))) ^^ {
            case (s1 @ STRING(_)) ~ _ ~ (s2 @ STRING(_)) => BasicOperation(ADD, s1, s2)
            case (n1 @ NUMBER(_)) ~ _ ~ (n2 @ NUMBER(_)) => BasicOperation(ADD, n1, n2)
            case (n1 @ NUMBER(_)) ~ _ ~ (n2 @ BasicOperation(_, _, _)) => BasicOperation(ADD, n1, n2)
            case (n1 @ BasicOperation(_, _, _)) ~ _ ~ (n2 @ NUMBER(_)) => BasicOperation(ADD, n1, n2)
        }
    }

    def multiplication = (number ~ MULTIPLY ~ number) ^^ {
        case (n1 @ NUMBER(_)) ~ _ ~ (n2 @ NUMBER(_)) => BasicOperation(MULTIPLY, n1, n2)
    }

    def division = (number ~ DIVIDE ~ number) ^^ {
        case (n1 @ NUMBER(_)) ~ _ ~ (n2 @ NUMBER(_)) => BasicOperation(DIVIDE, n1, n2)
    }

    def subtraction = (number ~ SUBTRACT ~ number) ^^ {
        case (n1 @ NUMBER(_)) ~ _ ~ (n2 @ NUMBER(_)) => BasicOperation(SUBTRACT, n1, n2)
    }

    private def identifier: Parser[IDENTIFIER] = accept("identifier", { case id @ IDENTIFIER(_) => id })
    private def varType: Parser[VAR_TYPE] = accept("var_type", { case id @ VAR_TYPE(_) => id })
    private def number: Parser[NUMBER] = accept("number", { case id @ NUMBER(_) => id })
    private def string: Parser[STRING] = accept("string", { case id @ STRING(_) => id })
    private def bool: Parser[BOOL] = accept("bool", { case id @ BOOL(_) => id })
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
