package trees
package cgtree

import java.util.Locale

// The source code of these should be provided by each target language
enum PrimFun:
  case Nat
  case Zat
  case Zat_Of_Nat
  case Str_Len
  case Str_Char_At
  case Str_Concat
  case Char
  case Char_To_Str
  case Char_Eq
  case Map_New
  case Map_Get
  case Map_Put
  case Map_Values
  case Set_New
  case Set_Add
  case Seq_New
  case Seq_Add
  case Seq_Map
  case Seq_Fold_Left
  case Pair_New
  case Pair_First
  case Pair_Second
  case Require
  case Ensure
end PrimFun

extension (primitive: PrimFun)
  // used for code generation
  def identifier: String = primitive.toString.toLowerCase(Locale.ENGLISH)
end extension

abstract class PrimFunVisitor:
  type Ctx = SourceCodeWriter
  type Result = Unit

  def visit_Map_New(pf: PrimFun, ctx: Ctx): Result
  def visit_Map_Get(pf: PrimFun, ctx: Ctx): Result
  def visit_Map_Put(pf: PrimFun, ctx: Ctx): Result
  def visit_Map_Values(pf: PrimFun, ctx: Ctx): Result
  def visit_Set_New(pf: PrimFun, ctx: Ctx): Result
  def visit_Set_Add(pf: PrimFun, ctx: Ctx): Result
  def visit_Seq_New(pf: PrimFun, ctx: Ctx): Result
  def visit_Seq_Add(pf: PrimFun, ctx: Ctx): Result
  def visit_Seq_Map(pf: PrimFun, ctx: Ctx): Result
  def visit_Seq_Fold_Left(pf: PrimFun, ctx: Ctx): Result
  def visit_Nat(pf: PrimFun, ctx: Ctx): Result
  def visit_Zat(pf: PrimFun, ctx: Ctx): Result
  def visit_Zat_Of_Nat(pf: PrimFun, ctx: Ctx): Result
  def visit_Str_Len(pf: PrimFun, ctx: Ctx): Result
  def visit_Str_Char_At(pf: PrimFun, ctx: Ctx): Result
  def visit_Str_Concat(pf: PrimFun, ctx: Ctx): Result
  def visit_Char(pf: PrimFun, ctx: Ctx): Result
  def visit_Char_To_Str(pf: PrimFun, ctx: Ctx): Result
  def visit_Char_Eq(pf: PrimFun, ctx: Ctx): Result
  def visit_Pair_New(pf: PrimFun, ctx: Ctx): Result
  def visit_Pair_First(pf: PrimFun, ctx: Ctx): Result
  def visit_Pair_Second(pf: PrimFun, ctx: Ctx): Result
  def visit_Require(pf: PrimFun, ctx: Ctx): Result
  def visit_Ensure(pf: PrimFun, ctx: Ctx): Result

  def visit(pf: PrimFun, ctx: Ctx): Result =
    pf match
      case pf @ PrimFun.Nat => visit_Nat(pf, ctx)
      case pf @ PrimFun.Zat => visit_Zat(pf, ctx)
      case pf @ PrimFun.Zat_Of_Nat => visit_Zat_Of_Nat(pf, ctx)
      case pf @ PrimFun.Str_Len => visit_Str_Len(pf, ctx)
      case pf @ PrimFun.Str_Char_At => visit_Str_Char_At(pf, ctx)
      case pf @ PrimFun.Str_Concat => visit_Str_Concat(pf, ctx)
      case pf @ PrimFun.Char => visit_Char(pf, ctx)
      case pf @ PrimFun.Char_To_Str => visit_Char_To_Str(pf, ctx)
      case pf @ PrimFun.Char_Eq => visit_Char_Eq(pf, ctx)
      case pf @ PrimFun.Map_New => visit_Map_New(pf, ctx)
      case pf @ PrimFun.Map_Get => visit_Map_Get(pf, ctx)
      case pf @ PrimFun.Map_Put => visit_Map_Put(pf, ctx)
      case pf @ PrimFun.Map_Values => visit_Map_Values(pf, ctx)
      case pf @ PrimFun.Set_New => visit_Set_New(pf, ctx)
      case pf @ PrimFun.Set_Add => visit_Set_Add(pf, ctx)
      case pf @ PrimFun.Seq_New => visit_Seq_New(pf, ctx)
      case pf @ PrimFun.Seq_Add => visit_Seq_Add(pf, ctx)
      case pf @ PrimFun.Seq_Map => visit_Seq_Map(pf, ctx)
      case pf @ PrimFun.Seq_Fold_Left => visit_Seq_Fold_Left(pf, ctx)
      case pf @ PrimFun.Pair_New => visit_Pair_New(pf, ctx)
      case pf @ PrimFun.Pair_First => visit_Pair_First(pf, ctx)
      case pf @ PrimFun.Pair_Second => visit_Pair_Second(pf, ctx)
      case pf @ PrimFun.Require => visit_Require(pf, ctx)
      case pf @ PrimFun.Ensure => visit_Ensure(pf, ctx)
    end match
  end visit
end PrimFunVisitor
