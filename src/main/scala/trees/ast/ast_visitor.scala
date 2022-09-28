package trees
package ast

import core.*
import trees.ast.AST

/**
 * @tparam Ctx A potentially mutable context passed around via the `visit_XYZ` methods.
 *             You are encouraged to place your visitor state here.
 * @tparam VisitResult An intermediate result of the individual `visit_XYZ` methods.
 * @tparam Result  The final result of this visitor.
 */
abstract class ASTVisitor[T, Ctx, VisitResult, Result](ast: AST[T], _logger: Logger):
  protected val logger = _logger

  def log_push_ctx(name: String, info: String = ""): Unit =
    if is_logging_on() then logger.push_ctx(name, info)
  end log_push_ctx

  def log_pop_ctx(info: String = ""): Unit =
    if is_logging_on() then logger.pop_ctx(info)
  end log_pop_ctx

  def is_logging_on(): Boolean = logger.is_logging_on

  def set_logging(logging: Logging): this.type =
    logger.set_logging(logging)
    this
  end set_logging

  def set_logging_off(): this.type =
    logger.set_logging_off()
    this
  end set_logging_off

  def set_logging_on(): this.type =
    logger.set_logging_on()
    this
  end set_logging_on

  def log(s: String): Unit = if is_logging_on() then logger.log(s.toString)

  def log_code(s: String): Unit = log(s"CODE: $s")

  def get_initial_ctx(): Ctx
  def get_result(ast: AST[T], vr: VisitResult, ctx: Ctx): Result

  def apply(): Result =
    val ctx = get_initial_ctx()
    val visit_result = visit(ast, Nil, ctx)
    val result = get_result(ast, visit_result, ctx)

    result
  end apply

  final def run(): Result = apply()

  //object Numbers:
  def visit_NatT(ast: AST.NatT, parents: ASTs, ctx: Ctx): VisitResult
  def visit_ZatT(ast: AST.ZatT, parents: ASTs, ctx: Ctx): VisitResult
  def visit_ZatOfNatT(ast: AST.ZatOfNatT, parents: ASTs, ctx: Ctx): VisitResult
  def visit_ArithBinOpT[T](ast: AST.ArithBinOpT[T], parents: ASTs, ctx: Ctx): VisitResult
  //end Numbers

  //object Bools:
  def visit_BoolT(ast: AST.BoolT, parents: ASTs, ctx: Ctx): VisitResult
  def visit_BoolBinOpT(ast: AST.BoolBinOpT, parents: ASTs, ctx: Ctx): VisitResult
  def visit_BoolNotT(ast: AST.BoolNotT, parents: ASTs, ctx: Ctx): VisitResult
  def visit_ComparisonBinOpT[T](ast: AST.ComparisonBinOpT[T], parents: ASTs, ctx: Ctx): VisitResult
  //end Bools

  //object Strings:
  def visit_StringT(ast: AST.StringT, parents: ASTs, ctx: Ctx): VisitResult
  def visit_StringLenT(ast: AST.StringLenT, parents: ASTs, ctx: Ctx): VisitResult
  def visit_StringCharAtT(ast: AST.StringCharAtT, parents: ASTs, ctx: Ctx): VisitResult
  def visit_StringConcatT(ast: AST.StringConcatT, parents: ASTs, ctx: Ctx): VisitResult
  def visit_CharT(ast: AST.CharT, parents: ASTs, ctx: Ctx): VisitResult
  def visit_CharToStrT(ast: AST.CharToStrT, parents: ASTs, ctx: Ctx): VisitResult
  def visit_CharEqT(ast: AST.CharEqT, parents: ASTs, ctx: Ctx): VisitResult
  //end Strings

  //object Binds:
  def visit_ParamT[T](ast: AST.ParamT[T], parents: ASTs, ctx: Ctx): VisitResult
  def visit_BindT[T](ast: AST.BindT[T], parents: ASTs, ctx: Ctx): VisitResult
  def visit_LetT[A, Z](ast: AST.LetT[A, Z], parents: ASTs, ctx: Ctx): VisitResult
  //end Binds

  //object Controls:
  def visit_IfThenElseT[T](ast: AST.IfThenElseT[T], parents: ASTs, ctx: Ctx): VisitResult
  def visit_ForLoopT(ast: AST.ForLoopT[?], parents: ASTs, ctx: Ctx): VisitResult
  //end Controls:

  //object Functions:
  def visit_Fun0T[Z](ast: AST.Fun0T[Z], parents: ASTs, ctx: Ctx): VisitResult
  def visit_App0T[Z](ast: AST.App0T[Z], parents: ASTs, ctx: Ctx): VisitResult
  def visit_Fun1T[A, Z](ast: AST.Fun1T[A, Z], parents: ASTs, ctx: Ctx): VisitResult
  def visit_App1T[A, Z](ast: AST.App1T[A, Z], parents: ASTs, ctx: Ctx): VisitResult
  def visit_Fun2T[A, B, Z](ast: AST.Fun2T[A, B, Z], parents: ASTs, ctx: Ctx): VisitResult
  def visit_App2T[A, B, Z](ast: AST.App2T[A, B, Z], parents: ASTs, ctx: Ctx): VisitResult
  def visit_Fun3T[A, B, C, Z](ast: AST.Fun3T[A, B, C, Z], parents: ASTs, ctx: Ctx): VisitResult
  def visit_App3T[A, B, C, Z](ast: AST.App3T[A, B, C, Z], parents: ASTs, ctx: Ctx): VisitResult
  def visit_Fun4T[A, B, C, D, Z](ast: AST.Fun4T[A, B, C, D, Z], parents: ASTs, ctx: Ctx): VisitResult
  def visit_App4T[A, B, C, D, Z](ast: AST.App4T[A, B, C, D, Z], parents: ASTs, ctx: Ctx): VisitResult
  def visit_Fun5T[A, B, C, D, E, Z](ast: AST.Fun5T[A, B, C, D, E, Z], parents: ASTs, ctx: Ctx): VisitResult
  def visit_App5T[A, B, C, D, E, Z](ast: AST.App5T[A, B, C, D, E, Z], parents: ASTs, ctx: Ctx): VisitResult
  //end Functions

  //object Maps:
  def visit_MapNewT[K, V](ast: AST.MapNewT[K, V], parents: ASTs, ctx: Ctx): VisitResult
  def visit_MapGetT[K, V](ast: AST.MapGetT[K, V], parents: ASTs, ctx: Ctx): VisitResult
  def visit_MapValuesT[K, V](ast: AST.MapValuesT[K, V], parents: ASTs, ctx: Ctx): VisitResult
  def visit_MapPutT[K, V](ast: AST.MapPutT[K, V], parents: ASTs, ctx: Ctx): VisitResult
  //end Maps

  //object Seqs:
  def visit_SeqNewT[K](ast: AST.SeqNewT[K], parents: ASTs, ctx: Ctx): VisitResult
  def visit_SeqAddT[K](ast: AST.SeqAddT[K], parents: ASTs, ctx: Ctx): VisitResult
  def visit_SeqMapT[K, L](ast: AST.SeqMapT[K, L], parents: ASTs, ctx: Ctx): VisitResult
  def visit_SeqFoldLeftT[K, L](ast: AST.SeqFoldLeftT[K, L], parents: ASTs, ctx: Ctx): VisitResult
  //end Seqs

  //object Sets:
  def visit_SetNewT[K](ast: AST.SetNewT[K], parents: ASTs, ctx: Ctx): VisitResult
  def visit_SetAddT[K](ast: AST.SetAddT[K], parents: ASTs, ctx: Ctx): VisitResult
  //end Sets

  //object OtherData:
  def visit_NoneT[T](ast: AST.NoneT[T], parents: ASTs, ctx: Ctx): VisitResult
  def visit_SomeT[T](ast: AST.SomeT[T], parents: ASTs, ctx: Ctx): VisitResult

  def visit_PairNewT[A, B](ast: AST.PairNewT[A, B], parents: ASTs, ctx: Ctx): VisitResult
  def visit_PairFirstT[A, B](ast: AST.PairFirstT[A, B], parents: ASTs, ctx: Ctx): VisitResult
  def visit_PairSecondT[A, B](ast: AST.PairSecondT[A, B], parents: ASTs, ctx: Ctx): VisitResult
  //end OtherData

  //object Structs:
  def visit_StructNewT(ast: AST.StructNewT[?, ?, ?], parents: ASTs, ctx: Ctx): VisitResult
  def visit_StructDotT(ast: AST.StructDotT[?, ?, ?], parents: ASTs, ctx: Ctx): VisitResult
  //end Structs

  //object Misc:
  def visit_TodoT(ast: AST.TodoT[?], parents: ASTs, ctx: Ctx): VisitResult
  //end Misc

  //object Props:
  def visit_PropT(ast: AST.PropT, parents: ASTs, ctx: Ctx): VisitResult
  //end Props

  def visit(ast: AST[?], parents: ASTs, ctx: Ctx): VisitResult =
    log_push_ctx(s"${getClass.getSimpleName}.${sourcecode.Name()}(${ast.to_short_str})")

    val result =
      ast match
        case tt : AST.NatT => visit_NatT(tt, parents, ctx)
        case tt : AST.ZatT => visit_ZatT(tt, parents, ctx)
        case tt : AST.ZatOfNatT => visit_ZatOfNatT(tt, parents, ctx)
        case tt : AST.ArithBinOpT[?] => visit_ArithBinOpT(tt, parents, ctx)

        case tt : AST.BoolT => visit_BoolT(tt, parents, ctx)
        case tt : AST.BoolBinOpT => visit_BoolBinOpT(tt, parents, ctx)
        case tt : AST.BoolNotT => visit_BoolNotT(tt, parents, ctx)
        case tt : AST.ComparisonBinOpT[?] => visit_ComparisonBinOpT(tt, parents, ctx)

        case tt : AST.StringT => visit_StringT(tt, parents, ctx)
        case tt : AST.StringLenT => visit_StringLenT(tt, parents, ctx)
        case tt : AST.StringCharAtT => visit_StringCharAtT(tt, parents, ctx)
        case tt : AST.StringConcatT => visit_StringConcatT(tt, parents, ctx)
        case tt : AST.CharT => visit_CharT(tt, parents, ctx)
        case tt : AST.CharToStrT => visit_CharToStrT(tt, parents, ctx)
        case tt : AST.CharEqT => visit_CharEqT(tt, parents, ctx)

        case tt : AST.ParamT[?] => visit_ParamT(tt, parents, ctx)
        case tt : AST.BindT[?] => visit_BindT(tt, parents, ctx)
        case tt : AST.LetT[?, ?] => visit_LetT(tt, parents, ctx)

        case tt : AST.IfThenElseT[?] => visit_IfThenElseT(tt, parents, ctx)
        case tt : AST.ForLoopT[?] => visit_ForLoopT(tt, parents, ctx)

        case tt : AST.Fun0T[?] => visit_Fun0T(tt, parents, ctx)
        case tt : AST.App0T[?] => visit_App0T(tt, parents, ctx)
        case tt : AST.Fun1T[?, ?] => visit_Fun1T(tt, parents, ctx)
        case tt : AST.App1T[?, ?] => visit_App1T(tt, parents, ctx)
        case tt : AST.Fun2T[?, ?, ?] => visit_Fun2T(tt, parents, ctx)
        case tt : AST.App2T[?, ?, ?] => visit_App2T(tt, parents, ctx)
        case tt : AST.Fun3T[?, ?, ?, ?] => visit_Fun3T(tt, parents, ctx)
        case tt : AST.App3T[?, ?, ?, ?] => visit_App3T(tt, parents, ctx)
        case tt : AST.Fun4T[?, ?, ?, ?, ?] => visit_Fun4T(tt, parents, ctx)
        case tt : AST.App4T[?, ?, ?, ?, ?] => visit_App4T(tt, parents, ctx)
        case tt : AST.Fun5T[?, ?, ?, ?, ?, ?] => visit_Fun5T(tt, parents, ctx)
        case tt : AST.App5T[?, ?, ?, ?, ?, ?] => visit_App5T(tt, parents, ctx)

        case tt : AST.MapNewT[?, ?] => visit_MapNewT(tt, parents, ctx)
        case tt : AST.MapGetT[?, ?] => visit_MapGetT(tt, parents, ctx)
        case tt : AST.MapValuesT[?, ?] => visit_MapValuesT(tt, parents, ctx)
        case tt : AST.MapPutT[?, ?] => visit_MapPutT(tt, parents, ctx)

        case tt : AST.SeqNewT[?] => visit_SeqNewT(tt, parents, ctx)
        case tt : AST.SeqAddT[?] => visit_SeqAddT(tt, parents, ctx)
        case tt : AST.SeqMapT[?, ?] => visit_SeqMapT(tt, parents, ctx)
        case tt : AST.SeqFoldLeftT[?, ?] => visit_SeqFoldLeftT(tt, parents, ctx)

        case tt : AST.SetNewT[?] => visit_SetNewT(tt, parents, ctx)
        case tt : AST.SetAddT[?] => visit_SetAddT(tt, parents, ctx)

        case tt : AST.NoneT[?] => visit_NoneT(tt, parents, ctx)
        case tt : AST.SomeT[?] => visit_SomeT(tt, parents, ctx)
        case tt : AST.PairNewT[?, ?] => visit_PairNewT(tt, parents, ctx)
        case tt : AST.PairFirstT[?, ?] => visit_PairFirstT(tt, parents, ctx)
        case tt : AST.PairSecondT[?, ?] => visit_PairSecondT(tt, parents, ctx)

        case tt : AST.StructNewT[?, ?, ?] => visit_StructNewT(tt, parents, ctx)
        case tt : AST.StructDotT[?, ?, ?] => visit_StructDotT(tt, parents, ctx)

        case tt : AST.TodoT[?] => visit_TodoT(tt, parents, ctx)

        case tt : AST.PropT => visit_PropT(tt, parents, ctx)
      end match

    log_pop_ctx()

    result
  end visit
end ASTVisitor

