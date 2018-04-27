package entry

import lexer.Lexer

import scala.io.Source

object Entry {
  def main(args: Array[String]) = {
    val code = Source
      .fromFile("src\\main\\scala\\entry\\Program.xd")
      .getLines
      .map(s => // isfiltruoja komentarus
        if (s.contains("#")) s.substring(0, s.indexOf("#"))
        else s
      )
      .filter(_.trim.nonEmpty)
      .mkString("")

    val tokens = Lexer(code) match { case Right(list) => list }
    val ast = parser.Parser(tokens)

    print("CODE: \n"  + code + "\n")
    print("\nTOKENS: " + tokens)
    print("\nAST: " + ast + "\n")

//    Lexer(code)
//    ()
  }
}
