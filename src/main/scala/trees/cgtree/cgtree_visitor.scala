package trees
package cgtree

import core.*

abstract class CGTreeVisitor[Ctx, Result]:
  def visit_LiteralExprT(tree: CGTree.LiteralExprT, ctx: Ctx): Result
  def visit_RefExprT(tree: CGTree.RefExprT, ctx: Ctx): Result
  def visit_BinOpExprT(tree: CGTree.BinOpExprT, ctx: Ctx): Result
  def visit_UnaryOpExprT(tree: CGTree.UnaryOpExprT, ctx: Ctx): Result
  def visit_CallExprT(tree: CGTree.CallExprT, ctx: Ctx): Result
  def visit_StructNewExprT(tree: CGTree.StructNewExprT, ctx: Ctx): Result
  def visit_DotExprT(tree: CGTree.DotExprT, ctx: Ctx): Result
  def visit_IfThenElseExprT(tree: CGTree.IfThenElseExprT, ctx: Ctx): Result

  def visit_ValDefT(tree: CGTree.ValDefT, ctx: Ctx): Result
  def visit_FunDefT(tree: CGTree.FunDefT, ctx: Ctx): Result
  def visit_LambdaDefT(tree: CGTree.LambdaDefT, ctx: Ctx): Result
  def visit_StructDefT(tree: CGTree.StructDefT, ctx: Ctx): Result

  def visit_PrimCodeT(tree: CGTree.PrimCodeT, ctx: Ctx): Result

  def visit_CommentT(tree: CGTree.CommentT, ctx: Ctx): Result
  
  def visit_ForLoopT(tree: CGTree.ForLoopT, ctx: Ctx): Result

  def visit_RequireT(tree: CGTree.RequireT, ctx: Ctx): Result

  def visit_EnsureT(tree: CGTree.EnsureT, ctx: Ctx): Result

  def visit(tree: CGTree, ctx: Ctx): Result =
    tree match
      case t: CGTree.LiteralExprT => visit_LiteralExprT(t, ctx)
      case t: CGTree.RefExprT => visit_RefExprT(t, ctx)
      case t: CGTree.BinOpExprT => visit_BinOpExprT(t, ctx)
      case t: CGTree.UnaryOpExprT => visit_UnaryOpExprT(t, ctx)
      case t: CGTree.CallExprT => visit_CallExprT(t, ctx)
      case t: CGTree.StructNewExprT => visit_StructNewExprT(t, ctx)
      case t: CGTree.DotExprT => visit_DotExprT(t, ctx)
      case t: CGTree.IfThenElseExprT => visit_IfThenElseExprT(t, ctx)

      case t: CGTree.ValDefT => visit_ValDefT(t, ctx)
      case t: CGTree.FunDefT => visit_FunDefT(t, ctx)
      case t: CGTree.LambdaDefT => visit_LambdaDefT(t, ctx)
      case t: CGTree.StructDefT => visit_StructDefT(t, ctx)

      case t: CGTree.PrimCodeT => visit_PrimCodeT(t, ctx)

      case t: CGTree.CommentT => visit_CommentT(t, ctx)
      case t: CGTree.ForLoopT => visit_ForLoopT(t, ctx)

      case t: CGTree.RequireT => visit_RequireT(t, ctx)
      case t: CGTree.EnsureT  => visit_EnsureT(t, ctx)
    end match
  end visit
end CGTreeVisitor
