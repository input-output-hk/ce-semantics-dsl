package trees
package cgtree

import core.*

import scala.annotation.targetName

// Just a marker trait for all the common, non-visitor behaviour
trait CodeGenBase:
  code_gen_base =>

  // Get all imports and primitive function definitions that the module needs.
  // In the future, when we support more than one (global) module,
  // we'll need to be more fine-grained in what we return here.
  def get_module_preamble(writer: SourceCodeWriter): CGTrees

  def write_comment(writer: SourceCodeWriter, comment: String): Unit

  def write_comment_ln(writer: SourceCodeWriter, comment: String): Unit =
    write_comment(writer, comment)
    writer.write_ln()
  end write_comment_ln

  // In Scala: "name: type" or "name_ : type"
  // In C    : "type name"
  def write_name_and_type(writer: SourceCodeWriter, name: String, ty: String): Unit

  def write_name_and_type(writer: SourceCodeWriter, name_type: NameType): Unit =
    write_name_and_type(writer, name_type.name, ty_to_str(name_type.ty))
  end write_name_and_type

  def write_function_params(writer: SourceCodeWriter, params: NameTypes): Unit

  // Note that there are no type parameters. We have not abstracted over generics yet,
  //      for user-defined functions that is. Every function with generics is a primitive one so far.
  def write_function_params_and_ret_ty(writer: SourceCodeWriter, params: NameTypes, ret_ty: Ty[?]): Unit

  extension (writer: SourceCodeWriter)
    @targetName("ext_writer_get_module_preamble")
    def get_module_preamble(): CGTrees = code_gen_base.get_module_preamble(writer)

    @targetName("ext_writer_write_comment")
    def write_comment(comment: String): Unit = code_gen_base.write_comment(writer, comment)

    @targetName("ext_writer_write_comment_ln")
    def write_comment_ln(comment: String): Unit = code_gen_base.write_comment_ln(writer, comment)

    @targetName("ext_writer_write_name_and_type/str,str")
    def write_name_and_type(name: String, ty: String): Unit = code_gen_base.write_name_and_type(writer, name, ty)

    @targetName("ext_writer_write_name_and_type/NameType")
    def write_name_and_type(name_type: NameType): Unit = code_gen_base.write_name_and_type(writer, name_type)

    @targetName("ext_writer_write_function_params")
    def write_function_params(params: NameTypes): Unit = code_gen_base.write_function_params(writer, params)

    @targetName("ext_writer_write_function_params_and_ret_ty")
    def write_function_params_and_ret_ty(params: NameTypes, ret_ty: Ty[?]): Unit =
      code_gen_base.write_function_params_and_ret_ty(writer, params, ret_ty)
    end write_function_params_and_ret_ty
  end extension

  def ty_to_str(ty: Ty[?]): String

  def binop_to_str(op: BinOp): String

  def unaryop_to_str(op: UnaryOp): String
end CodeGenBase
