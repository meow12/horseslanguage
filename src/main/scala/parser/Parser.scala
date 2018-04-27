package parser

import entry.CompilationError.{Location, ParserError}
import lexer._
import parser.Expression.{conditionalExpr, expression}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object Parser extends Parsers {
  override type Elem = Token

  class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = tokens.headOption.map{_.pos}.getOrElse(NoPosition)

    override def rest: Reader[Token] = new TokenReader(tokens.tail)
  }

  def apply(tokens: Seq[Token]): Either[ParserError, AST] = {
    val reader = new TokenReader(tokens)
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
    ifStatement | assignment | whileLoop | readLine | printLine | function
  }

  def assignment = newVarAssignment | existingVarAssignment

  def newVarAssignment: Parser[AST] = {
    (varType ~ identifier ~ ASSIGNMENT ~ expression).map {
      case varType ~ identifier ~ _ ~ value => NewAssignment(varType, identifier, value)
    }
  }

  def existingVarAssignment: Parser[AST] = {
    (identifier ~ ASSIGNMENT ~ expression).map {
      case identifier ~ _ ~ value => ExistingAssignment(identifier, value)
    }
  }

  def ifStatement: Parser[Statement] = {
    IF ~ LEFT_PARENTHESIS ~ conditionalExpr ~ RIGHT_PARENTHESIS ~ THEN ~ OPEN_BLOCK ~ block ~ CLOSE_BLOCK ~ opt(ELSE ~ OPEN_BLOCK ~ block ~ CLOSE_BLOCK) ^^ {
      case _ ~ _ ~ cond ~ _ ~ _ ~ _ ~ thenBlock ~ _ ~ elseOpt =>
        val elseBlock = elseOpt match { case Some(_ ~ _ ~ ast ~ _) => ast; case _ => EmptyAST }
        IfConditional(cond, thenBlock, elseBlock)
    }
  }

  def whileLoop: Parser[WhileLoop] = {
    WHILE ~ LEFT_PARENTHESIS ~ conditionalExpr ~ RIGHT_PARENTHESIS ~ OPEN_BLOCK ~ block ~ CLOSE_BLOCK ^^ {
      case _ ~ _ ~ cond ~ _ ~ _ ~ block ~ _ => WhileLoop(cond, block)
    }
  }

  def function: Parser[Function] = {
    (FUN ~ identifier ~ LEFT_PARENTHESIS ~ rep(varType ~ identifier <~ COMMA) ~
      varType ~ identifier ~ RIGHT_PARENTHESIS ~ OPEN_BLOCK ~ block ~ CLOSE_BLOCK).map {
      case _ ~ fname ~ _ ~ args ~ lastArgVartype ~ lastArg ~ _ ~ _ ~ body ~ _ =>
        val parsedArgs = (FuncArg(lastArgVartype, lastArg) :: args.map { case vartype ~ ident => FuncArg(vartype, ident) }).reverse
        Function(fname, parsedArgs, body)
    }
  }

  def readLine: Parser[ReadLine] = (READ ~> identifier <~ SEMICOLON).map(ReadLine)
  def printLine: Parser[PrintLine] = (PRINT ~> identifier <~ SEMICOLON).map(PrintLine)

  def value: Parser[Expression] = number | bool | string
  def identifier: Parser[IDENTIFIER] = accept("identifier", { case id @ IDENTIFIER(_) => id })
  def varType: Parser[VAR_TYPE] = accept("var_type", { case id @ VAR_TYPE(_) => id })
  def number: Parser[NUMBER] = accept("number", { case id @ NUMBER(_) => id })
  def string: Parser[STRING] = accept("string", { case id @ STRING(_) => id })
  def bool: Parser[BOOL] = accept("bool", { case id @ BOOL(_) => id })
}
