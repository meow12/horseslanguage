package compiler

import lexer.{Lexer, ProgramToken}

object TestCompiler {
    def main(args: Array[String]) = {
        val code =
            """
              |BOOL b2 = TRUE && FALSE || FALSE;
              |INT b = :one:two * :one - :two + :three;
              |STR s = "xd" + "ddddd";
            """.stripMargin.trim

//      BOOL b2 = TRUE && FALSE || FALSE;
//      INT b = :one:two * :one - :two + :three;
//      STR s = "xd" + "ddddd" + "Dasd";
        val ss = ProgramCompiler(code)
    }
}
