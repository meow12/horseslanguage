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
          case Success(result, next) => Right(result)
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

    private def assignment: Parser[AST] = (varType ~ identifier ~ ASSIGNMENT ~ ( number | bool | string ) <~ SEMICOLON).flatMap{
        case varType ~ identifier ~ _ ~ value  => (varType.value, value) match {
            case ("STR", STRING(s)) => success(StringAssignment(identifier.name, s))
            case ("BOOL", BOOL(b)) => success(BoolAssignment(identifier.name, b))
            case ("INT", NUMBER(i)) => success(NumberAssignment(identifier.name, i))
            case _ => failure("kintamojo tipas neatitinka kintamojo reiksmes: " + varType.value + " identifier = " + value)
        }
    }

    private def identifier: Parser[IDENTIFIER] = accept("identifier", { case id @ IDENTIFIER(name) => id })
    private def varType: Parser[VAR_TYPE] = accept("var_type", { case id @ VAR_TYPE(name) => id })
    private def number: Parser[NUMBER] = accept("number", { case id @ NUMBER(name) => id })
    private def string: Parser[STRING] = accept("string", { case id @ STRING(name) => id })
    private def bool: Parser[BOOL] = accept("bool", { case id @ BOOL(name) => id })

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
