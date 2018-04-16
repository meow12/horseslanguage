package parser

import lexer._
import scala.util.parsing.input.Positional

sealed trait AST extends Positional with Product with Serializable

case class NumberAssignment(name: String, value: Int) extends AST
case class StringAssignment(name: String, value: String) extends AST
case class BoolAssignment(name: String, value: Boolean) extends AST

case class BasicOperation(op: OPERAND, left: Value, right: Value) extends AST with Value
//case class Addition[A](left: Value[A], Left: Value[A]) extends Value[A] with AST
//case class Multiplication[A](left: Value[A], Left: Value[A]) extends Value[A] with AST
//case class Division[A](left: Value[A], Left: Value[A]) extends Value[A] with AST
//case class Subtraction[A](left: Value[A], Left: Value[A]) extends Value[A] with AST

case class AndThen(step1: AST, step2: AST) extends AST

//
//sealed trait ConditionThen extends AST { def thenBlock: AST }
//
//case class IfThen(predicate: Condition, thenBlock: AST) extends ConditionThen
//case class OtherwiseThen(thenBlock: AST) extends ConditionThen
//
//sealed trait Condition extends AST
//case class Equals(factName: String, factValue: String) extends Condition
