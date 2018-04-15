package compiler

import lexer.{Lexer, ProgramToken}
import parser.{AST, ProgramParser}

object ProgramCompiler {
  /*def apply(code: String): Either[CompilationError, AST] = {
    for {
      tokens <- Lexer(code).right
      ast <- ProgramParser(tokens).right
    } yield ast
  }*/

    def apply(code: String): Either[LexerError, List[ProgramToken]] = {
        print("CODE: \n"  + code + "\n")
        val tokens = Lexer(code) match {
            case Right(list) => list.toSeq
        }
        val ast = ProgramParser(tokens)
        print("\nTOKENS: " + tokens)
        print("\nAST: " + ast + "\n")

        Lexer(code)
    }
}
