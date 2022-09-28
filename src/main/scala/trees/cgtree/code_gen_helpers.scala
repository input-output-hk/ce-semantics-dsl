package trees

import core.*
import trees.CodeGenHelpers.struct_fields_as_NameTypes
import trees.ast.*
import trees.cgtree.*

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

case class CodeGenConfig(
  logger: Logger = new Logger(),
  include_ast_as_comment: Boolean = false
)

sealed class CodeGenHelpers:
  def write_Scala_comment(writer: SourceCodeWriter, comment: String): Unit =
    writer.write_code("//")
    writer.write_code(comment)
  end write_Scala_comment

  def write_Shell_comment(writer: SourceCodeWriter, comment: String): Unit =
    writer.write_code("#")
    writer.write_code(comment)
  end write_Shell_comment

  def param_as_NameType(param: AST.ParamT[?]): NameType =
    NameType(param.name, param.ty)
  end param_as_NameType

  def params_as_NameTypes(params: ParamTs): NameTypes =
    for param <- params yield param_as_NameType(param)
  end params_as_NameTypes

  def bare_fields_as_NameTypes(fields: List[BareField[?, ?]]): NameTypes =
    @tailrec
    def get_(fields: List[BareField[?, ?]], acc: ListBuffer[NameType]): NameTypes =
      fields match
        case Nil => acc.toList
        case field :: tail =>
          val name_ty = NameType(field.name, field.ty)
          acc += name_ty
          get_(tail, acc)
    end get_

    get_(fields, ListBuffer())
  end bare_fields_as_NameTypes

  inline def struct_bare_fields(struct_def: StructDef[?, ?, ?]): List[BareField[?, ?]] =
    struct_def.fields.toList.asInstanceOf[List[BareField[?, ?]]]
  end struct_bare_fields

  def struct_fields_as_NameTypes(struct_def: StructDef[?, ?, ?]): NameTypes =
    val fields = struct_bare_fields(struct_def)
    bare_fields_as_NameTypes(fields)
  end struct_fields_as_NameTypes

  def struct_field_names(struct_def: StructDef[?, ?, ?]): List[String] =
    struct_bare_fields(struct_def).map(_.name)
  end struct_field_names

  // CGTrees from structs
  // Note that we assume structs to be globally defined.
  //      Note also that we treat them asymmetrically with respect to ASTs, that is there is no AST
  //      for Struct definitions. There is though provision for them in the CGTRee representation.
  //      This is good enough for now.
  def gen_struct_CGTrees(struct_defs: StructDefs, config: CodeGenConfig): CGTrees =
    type Field = BareField[FieldName, ?]
    type Fields = List[Field]

    for struct_def <- struct_defs yield
      val name = struct_def.name
      val fields = struct_fields_as_NameTypes(struct_def)

      CGTree.StructDefT(name, fields)
    end for
  end gen_struct_CGTrees

  // CGTrees from AST
  def gen_ast_CGTrees(ast: AST[?], env: Env, config: CodeGenConfig): CGTrees =
    val logger = config.logger
    logger.push_ctx(s".${sourcecode.Name()}(ast = ${ast.to_typed_str}, scope = ${env})")

    val new_env = ast match
      case bind_t: AST.BindT[?] =>
        val _env = env.copy()
        _env.remove(bind_t)
        assert(! _env.contains(bind_t))

        _env
      case _ =>
        env

    val ctx = new ASTToCGTreeCtx(new_env, config.logger)
    val tree_gen = new ASTToCGTree(ast, ctx, config)

    val result = tree_gen.run()

    logger.pop_ctx()

    result
  end gen_ast_CGTrees

  def gen_source_code(trees: CGTrees, code_gen: CGTreeVisitorBase, writer: SourceCodeWriter): writer.type =
    val ctx = new CGTreeBaseCtx(writer)
    code_gen.visit_trees(trees, ctx)

    writer
  end gen_source_code

  def gen_source_code(ast: AST[?], scope: Env, config: CodeGenConfig, code_gen: CGTreeVisitorBase, writer: SourceCodeWriter): writer.type =
    val trees = gen_ast_CGTrees(ast, scope, config)
    gen_source_code(trees, code_gen, writer)
  end gen_source_code

  def gen_source_code_scala(trees: CGTrees, writer: SourceCodeWriter): writer.type =
    gen_source_code(trees, new CGTreeToScala(), writer)
  end gen_source_code_scala

  def gen_module(
    struct_defs: StructDefs,
    bindings: BindTs,
    code_gen: CGTreeVisitorBase,
    writer: SourceCodeWriter = SourceCodeWriter(),
    config: CodeGenConfig = CodeGenConfig()
  ): writer.type =
    val buffer = new CGTreeBuffer

    // Imports, primitives and other stuff that need to go before everything else
    val preamble_trees = code_gen.get_module_preamble(writer)
    buffer.add_trees(preamble_trees)

    buffer.add_tree(CGTree.CommentT("User-defined data and functions"))
    // Structs
    val struct_trees = gen_struct_CGTrees(struct_defs, config)
    buffer.add_trees(struct_trees)

    // Other code
    val env = RootEnv()
    env.add_all(bindings)

    for binding <- bindings do
      val bind_trees = gen_ast_CGTrees(binding, env, config)
      buffer.add_trees(bind_trees)
    end for

    // All trees computed so far
    val trees = buffer.get_trees()

    gen_source_code(trees, code_gen, writer)
  end gen_module
end CodeGenHelpers

object CodeGenHelpers extends CodeGenHelpers()
