package entry

import interpreter.Interpreter
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
      .mkString("\n")

    println("kodas:\n" + code)

    Lexer(code) match {
      case Left(err) => println(err)
      case Right(tokens) => parser.Parser(tokens) match {
        case Left(err) => println(err)
        case Right(ast) => Interpreter(ast)
      }
    }
  }
}
