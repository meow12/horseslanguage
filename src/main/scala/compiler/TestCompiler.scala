package compiler

import lexer.{Lexer, ProgramToken}

import scala.io.Source

object TestCompiler {
    def main(args: Array[String]) = {
      val code = Source
        .fromFile("src\\main\\scala\\compiler\\Program.xd")
        .getLines
        .map(s => // isfiltruoja komentarus
          if (s.contains("#")) s.substring(0, s.indexOf("#"))
          else s
        )
        .filter(_.trim.nonEmpty)
        .mkString("")

//      BOOL b2 = TRUE && FALSE || FALSE;
//      INT b = :one:two * :one - :two + :three;
//      STR s = "xd" + "ddddd" + "Dasd";
      ProgramCompiler(code)
      ()
    }
}
