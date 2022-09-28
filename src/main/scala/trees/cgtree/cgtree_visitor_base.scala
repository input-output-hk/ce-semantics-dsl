package trees
package cgtree

import core.*

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable.ListBuffer


class CGTreeBaseCtx(val writer: SourceCodeWriter):
  def get_code(): String = writer.get_code()

  def write_code(s: String): Unit = writer.write_code(s)

  def write_code_ln(s: String): Unit = writer.write_code_ln(s)

  def write_raw_str(s: String): Unit = writer.write_raw_str(s)

  def write_ln(): Unit = writer.write_ln()

  def with_indentation[C](closure: => C): C = writer.with_indentation(Indentation.More)(closure)

  def new_with_same_indentation(): CGTreeBaseCtx = CGTreeBaseCtx(writer.new_with_same_indentation())
end CGTreeBaseCtx

abstract class CGTreeVisitorBase extends CGTreeVisitor[CGTreeBaseCtx, Unit] with CodeGenBase:
  type Ctx = CGTreeBaseCtx
  type Result = Unit

  extension (ctx: Ctx)
    @targetName("ext_ctx_get_module_preamble")
    def get_module_preamble(): CGTrees = ctx.writer.get_module_preamble()

    @targetName("ext_ctx_write_comment")
    def write_comment(comment: String): Unit = ctx.writer.write_comment(comment)

    @targetName("ext_ctx_write_comment_ln")
    def write_comment_ln(comment: String): Unit = ctx.writer.write_comment_ln(comment)

    @targetName("ext_ctx_write_name_and_type/str,str")
    def write_name_and_type(name: String, ty: String): Unit = ctx.writer.write_name_and_type(name, ty)

    @targetName("ext_ctx_write_name_and_type/NameType")
    def write_name_and_type(name_type: NameType): Unit = ctx.writer.write_name_and_type(name_type)

    @targetName("ext_ctx_write_function_params")
    def write_function_params(params: NameTypes): Unit = ctx.writer.write_function_params(params)

    @targetName("ext_ctx_write_function_params_and_ret_ty")
    def write_function_params_and_ret_ty(params: NameTypes, ret_ty: Ty[?]): Unit =
      ctx.writer.write_function_params_and_ret_ty(params, ret_ty)
  end extension

  def visit_trees(trees: CGTrees, ctx: Ctx): Result =
    def same_tree_type(a: CGTree, b: CGTree): Bool = a.getClass eq b.getClass

    def render_trees(trees: CGTrees, ctx: Ctx, last_opt: Option[CGTree]): Unit =
      trees match
        case Nil =>
        case tree :: tail =>
          // Some pretty-printing love. The idea is that if the next tree is of different
          // type than the last one, then probably we have a different virtual "section", so
          // let's add an empty line to separate the sections.
          //  - Imports are such a virtual section
          //  - Vals are such a virtual section
          //  - Defs are such a virtual section
          //  - ...
          for last <- last_opt do
            if ! same_tree_type(last, tree) then ctx.write_ln()
          end for

          visit(tree, ctx)
          render_trees(tail, ctx, Some(tree))
      end match
    end render_trees

    render_trees(trees, ctx, None)
  end visit_trees
end CGTreeVisitorBase
