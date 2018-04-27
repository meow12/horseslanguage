package entry

sealed trait CompilationError extends Product with Serializable
object CompilationError {
  case class LexerError(location: Location, msg: String) extends CompilationError
  case class ParserError(location: Location, msg: String) extends CompilationError

  case class Location(line: Int, column: Int) { override def toString = s"$line:$column" }
}

