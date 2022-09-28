package examples
package report

// This is code that appears in the report.tex.
trait Core[R[_]]:
  def int(i: Int): R[Int]
  def bool(b: Boolean): R[Boolean]
  def not(b: R[Boolean]): R[Boolean]
  def add(a: R[Int], b: R[Int]): R[Int]
end Core

type Id[T] = T
trait DirectCore extends Core[Id]:
  def int(i: Int): Int = i
  def bool(b: Boolean): Boolean = b
  def not(b: Boolean): Boolean = !b
  def add(a: Int, b: Int): Int = a + b
end DirectCore

trait App[R[_]] extends Core[R]:
  val one: R[Int] = int(1)
  val two: R[Int] = add(one, one)

  val True: R[Boolean] = bool(true)
  val False: R[Boolean] = not(True)
end App

object DirectApp extends App[Id] with DirectCore

@main def print_direct_two() =
  println(DirectApp.two)
end print_direct_two

enum AST[T]:
  case IntT(i: Int) extends AST[Int]
  case BoolT(b: Boolean) extends AST[Boolean]
  case AddT(a: AST[Int], b: AST[Int]) extends AST[Int]
  case NotT(b: AST[Boolean]) extends AST[Boolean]
end AST

trait ASTCore extends Core[AST]:
  import AST.*

  def int(i: Int): AST[Int] = IntT(i)
  def bool(b: Boolean): AST[Boolean] = BoolT(b)
  def not(b: AST[Boolean]): AST[Boolean] = NotT(b)
  def add(a: AST[Int], b: AST[Int]): AST[Int] = AddT(a, b)
end ASTCore

object ASTApp extends App[AST] with ASTCore

@main def print_ast_two() =
  println(ASTApp.two)
end print_ast_two
