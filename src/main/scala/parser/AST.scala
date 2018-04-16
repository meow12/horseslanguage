package parser


import scala.util.parsing.input.Positional

sealed trait AST extends Positional with Product with Serializable

case class NumberAssignment(name: String, value: Int) extends AST
case class StringAssignment(name: String, value: String) extends AST
case class BoolAssignment(name: String, value: Boolean) extends AST

case class AndThen(step1: AST, step2: AST) extends AST
