package compiler

import lexer.{Lexer, ProgramToken}

object TestCompiler {
    def main(args: Array[String]) = {
        val code =
            """
              |STR a = "aaa";
              |INT b = :one:two;
              |BOOL c = TRUE;
              |BOOL c = TRUE;
            """.stripMargin.trim

        val ss = ProgramCompiler(code)
    }
}
