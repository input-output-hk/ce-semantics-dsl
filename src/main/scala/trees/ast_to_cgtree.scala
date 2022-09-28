package trees

import core.*
import chain.*
import chain.demo.*
import trees.ast.*
import trees.ast.AST
import trees.cgtree.*
import trees.cgtree.CGTree
import scala.annotation.tailrec

// Transforms the front-end AST to CGTree, which is more suitable for code generation.
class ASTToCGTree[T](
  ast: AST[T],
  initial_ctx: ASTToCGTreeCtx,
  config: CodeGenConfig = CodeGenConfig()
) extends ASTVisitor[T, ASTToCGTreeCtx, Fragment, CGTrees](
  ast,
  config.logger
):

  type Ctx = ASTToCGTreeCtx
  
  def get_initial_ctx(): Ctx = initial_ctx

  def get_result(ast: AST[T], vr: Fragment, ctx: Ctx): CGTrees =
    ctx.add_tree(vr)
    // get ir and all previous frtrees that are the dependencies necessary for ir.
    ctx.get_my_trees()
  end get_result

  private object Helpers:
    def mk_ref_from_synthetic_bind(bind_t: AST.BindT[?]): CGTree.RefExprT =
      require(bind_t.creation == BindTCreation.BindTRefExprT)
      CGTree.RefExprT(bind_t)
    end mk_ref_from_synthetic_bind

    def mk_synthetic_bind_from_primitive[T](primitive: PrimFun, ast: AST[T]): AST.BindT[T] =
      AST.BindT(primitive.identifier, ast, ast.ty, BindTCreation.BindTRefExprT): AST.BindT[T]
    end mk_synthetic_bind_from_primitive

    def mk_ref_from_primitive[T](primitive: PrimFun, ast: AST[T]): CGTree.RefExprT =
      val bind_t = mk_synthetic_bind_from_primitive(primitive, ast)
      mk_ref_from_synthetic_bind(bind_t)
    end mk_ref_from_primitive

    def mk_synthetic_bind_from_struct_new[T <: AnyStruct](ast: AST.StructNewT[T, ?, ?]): AST.BindT[T] =
      AST.BindT[T](
        ast.struct_def.name,
        ast,
        ast.ty,
        BindTCreation.BindTRefExprT
      ): AST.BindT[T]
    end mk_synthetic_bind_from_struct_new

    def mk_ref_from_struct_new(ast: AST.StructNewT[?, ?, ?]): CGTree.RefExprT =
      ast
        |> mk_synthetic_bind_from_struct_new
        |> mk_ref_from_synthetic_bind
    end mk_ref_from_struct_new

    def mk_synthetic_bind_from_param[T](ast: AST.ParamT[T]): AST.BindT[T] =
      AST.BindT[T](ast.name, ast, ast.ty, BindTCreation.BindTRefExprT): AST.BindT[T]
    end mk_synthetic_bind_from_param

    def mk_property_name(function_name: String, prop_is: PropIs, prop_index: Int): String =
      require(prop_index >= 0)
      s"${function_name}_${prop_is.source_code_identifier}${prop_index}"
    end mk_property_name

    def mk_ref_from_param(ast: AST.ParamT[?]): CGTree.RefExprT =
      ast
        |> mk_synthetic_bind_from_param
        |> mk_ref_from_synthetic_bind
    end mk_ref_from_param

    def mk_function_call(f: CGTree.RefExprT, ast: AST[?], types: Tys, params: ASTs, parents: ASTs, ctx: Ctx): CGTree =
      val param_expr_ts = for param <- params yield visit(param, ast :: parents, ctx)

      CGTree.CallExprT(
        f,
        types,
        param_expr_ts
      )
    end mk_function_call

    def mk_primitive_call(primitive: PrimFun, ast: AST[?], types: Tys, params: ASTs, parents: ASTs, ctx: Ctx): CGTree =
      val f = mk_ref_from_primitive(primitive, ast)

      mk_function_call(f, ast, types, params, parents, ctx)
    end mk_primitive_call

    def mk_binop(op: BinOp, a: AST[?], b: AST[?], parents: ASTs, ctx: Ctx): CGTree =
      val expr_a = visit(a, ast :: parents, ctx)
      val expr_b = visit(b, ast :: parents, ctx)

      CGTree.BinOpExprT(op, expr_a, expr_b)
    end mk_binop

    def debug_bindings(prefix: String, bindTs: BindTs, scope_owner: Scope): Unit =
      if(bindTs.nonEmpty) then
        log(s"In scope: ${scope_owner}:")
        val bind_t_names = bindTs.map(_.name).mkString(", ")
        log(s"  ${prefix} [${bindTs.length}]: $bind_t_names")
        //log(s"    ${bind_t_names}:")

        for bind_t <- bindTs do
          log(s"    ${bind_t.to_typed_str}")
        end for
      end if
    end debug_bindings

    // Generate code for the dependencies of `ast` at the same lexical scope.
    def gen_and_add_dep_code(ast: AST[?], parents: ASTs, ctx: Ctx): Unit =
      // optimization, also avoid cluttering the log
      if ast.isInstanceOf[AST.ParamT[?]] then return

      log_push_ctx(s"${sourcecode.Name()}(${ast}, parent = ${parents.head.to_short_str})")

      // 1. find BindTs in the `ast`
      val dependencies = FindBindTs(ast, logger).run()
      val unknown_deps = dependencies.filterNot(ctx.contains)
      val known_deps = ctx.get_all_bindings()
      //assert(dependencies.length <= (unknown_deps.length + known_deps.length))

      // 2. Generate code for the unknown dependencies
      if unknown_deps.nonEmpty then
        debug_bindings("Found deps", dependencies, ctx.scope_owner)
        debug_bindings("Known deps", known_deps, ctx.scope_owner)
        debug_bindings("Unknown deps", unknown_deps, ctx.scope_owner)

        log_push_ctx("[[for unknown_dep <- unknown_deps]]")

        for unknown_dep <- unknown_deps do
          val dep_ctx =  ctx.new_with_env_copy()

          log(s"Created new ctx: ${dep_ctx}")
          log(s"to gen. code for dependency: ${unknown_dep}")

          val unknown_dep_fragment = visit(unknown_dep, ast :: parents, dep_ctx)

          log(s"Generated tree: ${unknown_dep_fragment}")
          log(s"for unknown_dep: ${unknown_dep}")

          if dep_ctx.get_my_trees().nonEmpty then
            log(s"Dependency trees to be added are:")
            for extra_tree <- dep_ctx.get_my_trees() do
              log(s"  ${extra_tree}")
            end for

            //ctx.add_tree(CGTree.CommentT("Some dependencies first"))
            ctx.add_trees(dep_ctx.get_my_trees())
            //ctx.add_tree(CGTree.CommentT("... and the actual calculation"))
          end if

          ctx.add_tree(unknown_dep_fragment)

          // 3. Now this dependency is defined.
          //    Add it to the scope, so that it is not re-defined if we see it again
          ctx.add_binding(unknown_dep)
        end for

        log_pop_ctx()
      end if

      log_pop_ctx()
    end gen_and_add_dep_code

    // bind_t = BindT(name, fun_t = FunT(..., f_applied, ...))
    def gen_FunT_code_and_deps(params: ParamTs, fun_t: FunT, f_applied: () => AST[?], parents: ASTs, ctx: Ctx): CGTrees =
      ctx.do_with_new_FunT_ctx(fun_t, None) { fun_t_ctx =>
        f_applied() match
          // Walk a chain of FunTs ...
          // bind_t = BindT(name, fun_t = FunT(..., f_applied = FunT(...), ...))
          case FunT(nested_fun_t, nested_params, nested_f_applied, _) =>
            gen_FunT_code_and_deps(nested_params, nested_fun_t, nested_f_applied, fun_t :: parents, fun_t_ctx)
            ctx.add_bindings_of(fun_t_ctx)

          case tt : AST.BindT[?] =>
            gen_and_add_dep_code(fun_t, parents, fun_t_ctx)
            ctx.add_bindings_of(fun_t_ctx)

            // If the definition of FunT is a BindT, then this means the last expression is the bound variable.
            // Note that we have a genuine BindT == coming from the eDSL, and not a synthetic BindT; that's
            //      why we create a RefExprT directly and do not use "mk_synthetic_bind_t_from_XYZ".
            // TODO Review the need for synthetic binds and maybe use some other construct (than a BindT) in
            //      a RefExprT.
            fun_t_ctx.add_tree(CGTree.RefExprT(tt))

          case _ =>
            gen_and_add_dep_code(fun_t, parents, fun_t_ctx)
            ctx.add_bindings_of(fun_t_ctx)

            // just generate the code for the function body
            val body = visit(f_applied(), fun_t :: parents, fun_t_ctx)
            fun_t_ctx.add_tree(body)
        end match

        // now everything should be in place
        fun_t_ctx.get_my_trees()
      }
    end gen_FunT_code_and_deps

    def transform_last_to_prop_statement(prop_is: PropIs, trees: CGTrees): CGTrees =
      if trees.isEmpty then return trees
      val length = trees.length
      val first = trees.take(length - 1)
      val last  = trees.last.to_prop_statement(prop_is)

      first appended last      
    end transform_last_to_prop_statement
    
    def gen_CGProp(
      original_bind_t: AST.BindT[?], // this is the bind_t of the function for which we declare a property to be checked
      prop_is: PropIs,
      prop_ast: AST[?], // This is the AST of the actual property. See the below comment in the pattern match for `prop_ast`.
      prop_index: Int,
      parents: ASTs, ctx: Ctx
    ): CGProp =
      prop_ast match
        // Note The eDSL encodes all properties (pre-/post-conditions) as functions (via `bind_add_pre_condX` etc),
        //      so we expect a FunT here. Also, we re-use the code generation facilities we have for functions
        //      (`gen_FunT_code_and_deps`). Some transformations on the AST are still needed before we get the final
        //      result.
        case tt @ FunT(prop_fun_t, prop_f_params, prop_f_applied, prop_ty) =>
          val prop_owner_name = original_bind_t.name // e.g. leftpad
          val prop_name = mk_property_name(prop_owner_name, prop_is, prop_index) // e.g. leftpad_pre_cond1
          val prop_input = CodeGenHelpers.params_as_NameTypes(prop_f_params) // all input to leftpad, + its result if this is a post-condition
          val ret_ty = prop_f_applied().ty

          prop_f_applied() match
            case AST.PropT(prop) =>
              prop match
                case Prop.BoolP(predicate) =>
                  val pred_ty = predicate.ty
                  require(
                    pred_ty == Ty.BoolTy,
                    s"Internal error: Predicate type should be ${ Ty.BoolTy } but got ${ pred_ty }"
                  )

                  // Note how we synthesize a new body for the function, essentially discarding the `AST.PropT`,
                  //      which is pattern matched above.
                  //      This of course is a source for bugs, since we do not treat code (CGTree) generation for the
                  //      AST.PropT in a uniform way: Just try to write a function that returns a `prop_bool(true)` and
                  //      see what code is generated. See also the comments/docs for `prop_bool`, which state that you
                  //      should not use it directly.
                  //
                  // Go from: AST.FunT(..., () => AST.PropT(Prop.BoolP(predicate)), ...)
                  //      to: () => predicate
                  val synthetic_prop_f_applied = () => predicate
                  val cg_prop_body = gen_FunT_code_and_deps(prop_f_params, prop_fun_t, synthetic_prop_f_applied, tt :: parents, ctx)
                  val cg_prop_body_length = cg_prop_body.length
                  require(cg_prop_body_length > 0)

                  // now we need to transform the last CGTree node to a statement (while now it is a boolean condition)
                  // the last one is a boolean expression because is comes directly from `predicate`, and we have already
                  // validated `predicate`'s type as `Ty.BoolTy` (check the `require` right after the `Prop.BoolP`
                  // pattern match above).
                  val cg_predicate = transform_last_to_prop_statement(prop_is, cg_prop_body)

                  val cg_prop = CGProp.BoolCGP(
                    prop_is,
                    prop_owner_name,
                    prop_name,
                    prop_index,
                    prop_input,
                    cg_predicate
                  )

                  cg_prop

                case Prop.ForAllInRangeP(var_name, var_ty, var_bounds_ast, upper_bound_is, predicate_) =>
                  // By design, `predicate` is a `Fun1T`, a function expressing the (bounded) universal quantifier over
                  // a variable.
                  //
                  // Note I prefer an `IllegalArgumentException` to a `ClassCastException`.
                  //      The former clearly indicates a pre-condition violation.
                  // Note Flow typing would be nice here.
                  require(predicate_.isInstanceOf[AST.Fun1T[?,?]])
                  val predicate = predicate_.asInstanceOf[AST.Fun1T[?,?]]

                  val (cg_var_bounds, cg_bounds_trees, cg_loop_body_trees) =
                    // We need a new context in order to gather everything together
                    ctx.do_with_new_FunT_ctx(prop_fun_t, None) { prop_fun_t_ctx =>
                      // Generate dependency code for the bounds
                      println(s"++ var_bounds_ast._1 = ${var_bounds_ast._1}")
                      gen_and_add_dep_code(var_bounds_ast._1, prop_ast :: parents, prop_fun_t_ctx)
                      gen_and_add_dep_code(var_bounds_ast._2, prop_ast :: parents, prop_fun_t_ctx)
                      
                      val cg_lower_bound = visit(var_bounds_ast._1, prop_ast :: parents, prop_fun_t_ctx)
                      println(s"++ cg_lower_bound = ${cg_lower_bound}")
                      println(s"++ prop_fun_t_ctx = ${prop_fun_t_ctx}")
                      println(s"++ var_bounds_ast._1 = ${var_bounds_ast._1}")
                      val cg_upper_bound = visit(var_bounds_ast._2, prop_ast :: parents, prop_fun_t_ctx)
                      println(s"++ cg_upper_bound = ${cg_upper_bound}")
                      println(s"++ prop_fun_t_ctx = ${prop_fun_t_ctx}")
                      val cg_var_bounds = (cg_lower_bound, cg_upper_bound)

                      // generate code for the body of the for loop.
                      val cg_loop_body_trees = prop_fun_t_ctx.do_with_new_FunT_ctx(predicate, None) { loop_body_ctx =>
                        val loop_body = predicate.f_applied() // finally a boolean expression
                        
                        gen_and_add_dep_code(loop_body, predicate :: parents, loop_body_ctx)
                        val cg_loop_body = visit(loop_body, predicate :: parents, loop_body_ctx)
                        val cg_loop_body_deps = loop_body_ctx.get_my_trees()

                        cg_loop_body_deps appended cg_loop_body
                      }

                      // Now we have the code that is needed for the loop bounds
                      // and the code of the loop body itself,
                      // both with proper dependencies.
                      val cg_bounds_trees = prop_fun_t_ctx.get_my_trees()

                      (cg_var_bounds, cg_bounds_trees, cg_loop_body_trees)
                    }

                  val (cg_lower_bound, cg_upper_bound) = cg_var_bounds
                  val cg_transformed_loop_body_trees = transform_last_to_prop_statement(prop_is, cg_loop_body_trees)

                    // create the for loop
                  val cg_for_loop: CGTree.ForLoopT = CGTree.ForLoopT(
                    var_name,
                    cg_lower_bound,
                    cg_upper_bound,
                    upper_bound_is,
                    cg_transformed_loop_body_trees
                  )

                  // bundle everything together
                  val cg_prop = CGProp.ForAllInRangeCGP(
                    prop_is,
                    prop_owner_name,
                    prop_name,
                    prop_index,
                    prop_input,
                    cg_bounds_trees,
                    cg_for_loop
                  )

                  cg_prop
              end match
            case _ =>
              assert(false)
          end match
        case _ =>
          assert(false)
      end match
    end gen_CGProp

    // bind_t = BindT(name, value = fun_t = FunT(..., f_applied, ...))
    def gen_FunDefT(
      params: ParamTs,
      fun_t: FunT,
      fun_t_kind: FunTKind,
      f_applied: () => AST[?],
      bind_t: AST.BindT[?],
      parents: ASTs, ctx: Ctx
    ): CGTree.FunDefT =
      log_push_ctx(s"${getClass.getSimpleName}.${sourcecode.Name()}(${ast.to_short_str})")

      val name = bind_t.name
      val params_namety = CodeGenHelpers.params_as_NameTypes(params)
      val ret_ty = f_applied().ty

      val body = gen_FunT_code_and_deps(params, fun_t, f_applied, bind_t :: parents, ctx)

      // We now generate two more functions:
      //   - One that runs the pre-conditions, and
      //   - A second one that runs the post-conditions
      // Note Code generators may want to inline the functions at use-site.
      val pre_cond_f =
        for (prop, prop_index) <- bind_t.pre_cond.zipWithIndex yield
          gen_CGProp(bind_t, PropIs.PreCondition, prop, prop_index + 1, bind_t :: parents, ctx)
        end for

      val post_cond_f =
        for (prop, prop_index) <- bind_t.post_cond.zipWithIndex yield
          gen_CGProp(bind_t, PropIs.PostCondition, prop, prop_index + 1, bind_t :: parents, ctx)

      val result: CGTree.FunDefT =
        CGTree.FunDefT(
          name,
          params_namety,
          ret_ty,
          body,
          InlineIs.NotInline,
          pre_cond_f,
          post_cond_f
        )

      log_pop_ctx()

      result
    end gen_FunDefT

    // bind_t = BindT(name, value = XYZ), where XYZ is not a FunT
    def gen_ValDefT(bind_t: AST.BindT[?], parents: ASTs, ctx: Ctx): CGTree =
      log_push_ctx(s"${getClass.getSimpleName}.${sourcecode.Name()}(${ast.to_short_str})")

      // 1. find dependencies of the definition
      val val_value = bind_t.value
      gen_and_add_dep_code(val_value, bind_t :: parents, ctx)

      // Fun fact of the day: U+00B7 · MIDDLE DOT
      // ... but also check : U+22C5 ⋅ DOT OPERATOR

      // [·] shows what we focus on or what we compute, so [name] means we focus on the name
      // val [name] : type = value
      val val_name = bind_t.name
      // val name : [type] = value
      val val_type = val_value.ty
      val name_and_ty = NameType(val_name, val_type)

      // val name : type = [value]
      val val_value_expr = visit(val_value, bind_t :: parents, ctx)

      // Top-level vals are defined as lazy to avoid initialization issues.
      // Vals in functions are derived based on dependencies, so they should be OK,
      // meaning we do not need to make them lazy.
      val is_lazy = parents.isEmpty
      val result = CGTree.ValDefT(name_and_ty, val_value_expr, is_lazy)

      log_pop_ctx()

      result
    end gen_ValDefT
  end Helpers
  import Helpers.*

  //object Numbers:
  def visit_NatT(ast: AST.NatT, parents: ASTs, ctx: Ctx): Fragment =
    CGTree.LiteralExprT(Literal.NatLit(ast.nat))
  end visit_NatT

  def visit_ZatT(ast: AST.ZatT, parents: ASTs, ctx: Ctx): Fragment =
    CGTree.LiteralExprT(Literal.ZatLit(ast.zat))
  end visit_ZatT

  def visit_ZatOfNatT(ast: AST.ZatOfNatT, parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    mk_primitive_call(
      PrimFun.Zat_Of_Nat,
      ast,
      Nil,
      List(ast.nat),
      parents, ctx
    )
  end visit_ZatOfNatT

  def visit_ArithBinOpT[T](ast: AST.ArithBinOpT[T], parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    mk_binop(ast.op, ast.a, ast.b, parents, ctx)
  end visit_ArithBinOpT
  //end Numbers

  //object Bools:
  def visit_BoolT(ast: AST.BoolT, parents: ASTs, ctx: Ctx): Fragment =
    CGTree.LiteralExprT(Literal.BoolLit(ast.bool))
  end visit_BoolT

  def visit_BoolBinOpT(ast: AST.BoolBinOpT, parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    mk_binop(ast.op, ast.a, ast.b, parents, ctx)
  end visit_BoolBinOpT

  def visit_BoolNotT(ast: AST.BoolNotT, parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    val expr_a = visit(ast.a, ast :: parents, ctx)
    CGTree.UnaryOpExprT(BoolUnaryOp.Not, expr_a)
  end visit_BoolNotT

  def visit_ComparisonBinOpT[T](ast: AST.ComparisonBinOpT[T], parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    mk_binop(ast.op, ast.a, ast.b, parents, ctx)
  end visit_ComparisonBinOpT
  //end Bools

  //object Strings:
  def visit_StringT(ast: AST.StringT, parents: ASTs, ctx: Ctx): Fragment =
    CGTree.LiteralExprT(Literal.StringLit(ast.str))
  end visit_StringT

  def visit_StringLenT(ast: AST.StringLenT, parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    mk_primitive_call(
      PrimFun.Str_Len,
      ast,
      Nil,
      List(ast.str),
      parents, ctx
    )
  end visit_StringLenT

  def visit_StringCharAtT(ast: AST.StringCharAtT, parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    mk_primitive_call(
      PrimFun.Str_Char_At,
      ast,
      Nil,
      List(ast.str, ast.index),
      parents, ctx
    )
  end visit_StringCharAtT

  def visit_StringConcatT(ast: AST.StringConcatT, parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    mk_primitive_call(
      PrimFun.Str_Concat,
      ast,
      Nil,
      List(ast.a, ast.b),
      parents, ctx
    )
  end visit_StringConcatT

  def visit_CharT(ast: AST.CharT, parents: ASTs, ctx: Ctx): Fragment =
    CGTree.LiteralExprT(Literal.CharLit(ast.c))
  end visit_CharT

  def visit_CharToStrT(ast: AST.CharToStrT, parents: ASTs, ctx: Ctx): Fragment =
    mk_primitive_call(
      PrimFun.Char_To_Str,
      ast,
      Nil,
      List(ast.c),
      parents, ctx
    )
  end visit_CharToStrT

  def visit_CharEqT(ast: AST.CharEqT, parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    mk_primitive_call(
      PrimFun.Char_Eq,
      ast,
      Nil,
      List(ast.a, ast.b),
      parents, ctx
    )
  end visit_CharEqT
  //end Strings

  //object Binds:
  def visit_ParamT[T](ast: AST.ParamT[T], parents: ASTs, ctx: Ctx): Fragment =
    mk_ref_from_param(ast)
  end visit_ParamT

  def visit_BindT[T](bind_t: AST.BindT[T], parents: ASTs, ctx: Ctx): Fragment =
    if ctx.contains(bind_t) then
      log(s"Definition of `${bind_t.name}` is known, returning a ref.")
      return CGTree.RefExprT(bind_t)
    else
      log(s"Definition of `${bind_t.name}` is unknown, proceeding to generate code.")
      // Add it now, in case this binding is recursive.
      ctx.add_binding(bind_t)
    end if

    if config.include_ast_as_comment then
      val tree = CGTree.CommentT(bind_t.toString)
      ctx.add_tree(tree)
    end if

    val result =
      bind_t.value match
        case FunT(fun_t, params, f_applied, _) =>
          // def f()/f(x)/f(x, y) = ... end f
          gen_FunDefT(params, fun_t, FunTKind.RegularFunT, f_applied, bind_t, parents, ctx)
        case _ =>
          // val x : type = value
          gen_ValDefT(bind_t, parents, ctx)
      end match

    result
  end visit_BindT

  def visit_LetT[A, Z](ast: AST.LetT[A, Z], parents: ASTs, ctx: Ctx): Fragment =
    // TODO
    CGTree.CommentT(ast.toString)
  end visit_LetT
  //end Binds

  //object Controls:
  def visit_IfThenElseT[T](ast: AST.IfThenElseT[T], parents: ASTs, ctx: Ctx): Fragment =
    // Note This generates all dependencies before generating IF/THEN/ELSE code but in the originating semantics (spec)
    //      the dependencies could be inside THEN or ELSE. So, we need a more elaborate analysis in order to be
    //      "semantically" correct.
    //
    // TODO Analyze and design the more "correct" translation.
    gen_and_add_dep_code(ast, parents, ctx)

    val cg_if   = visit(ast._if, ast :: parents, ctx)
    // TODO there may be more than one trees as the THEN branch, so we need a new ctx
    val cg_then = visit(ast._then, ast :: parents, ctx)
    // TODO there may be more than one trees as the ELSE branch, so we need a new ctx
    val cg_else = visit(ast._else, ast :: parents, ctx)

    // TODO CGTrees vs CGTree
    CGTree.IfThenElseExprT(cg_if, cg_then, cg_else)
  end visit_IfThenElseT

  def visit_ForLoopT(ast: AST.ForLoopT[?], parents: ASTs, ctx: Ctx): Fragment =
    // Note See comments for visit_IfThenElseT regarding dependencies
    gen_and_add_dep_code(ast, parents, ctx)

    val cg_lower = visit(ast.lower, ast :: parents, ctx)
    val cg_upper = visit(ast.upper, ast :: parents, ctx)
    val cg_body = visit(ast.body, ast :: parents, ctx)

    CGTree.ForLoopT(ast.var_name, cg_lower, cg_upper, ast.upper_bound_is, cg_body :: Nil)
  end visit_ForLoopT
//end Controls:

  //object Functions:
  // TODO implement
  // Note Missing implementations are going to be a problem the moment we use anonymous (= non-BindT) functions.
  def visit_Fun0T[A](ast: AST.Fun0T[A], parents: ASTs, ctx: Ctx): Fragment = ???
  def visit_Fun1T[A, Z](ast: AST.Fun1T[A, Z], parents: ASTs, ctx: Ctx): Fragment =
    val params: ParamTs = List(ast.a)
    val params_namety = CodeGenHelpers.params_as_NameTypes(params)
    val ret_ty = ast.f_applied().ty
    val body = gen_FunT_code_and_deps(params, ast, ast.f_applied, ast :: parents, ctx) // or should it be just `parents` instead of `ast :: parents`?

    CGTree.LambdaDefT(
      params_namety,
      ret_ty,
      body
    )
  end visit_Fun1T

  def visit_Fun2T[A, B, Z](ast: AST.Fun2T[A, B, Z], parents: ASTs, ctx: Ctx): Fragment = ???
  def visit_Fun3T[A, B, C, Z](ast: AST.Fun3T[A, B, C, Z], parents: ASTs, ctx: Ctx): Fragment = ???
  def visit_Fun4T[A, B, C, D, Z](ast: AST.Fun4T[A, B, C, D, Z], parents: ASTs, ctx: Ctx): Fragment = ???
  def visit_Fun5T[A, B, C, D, E, Z](ast: AST.Fun5T[A, B, C, D, E, Z], parents: ASTs, ctx: Ctx): Fragment = ???

  def visit_AppTData[A](app_t: AppTData, parents: ASTs, ctx: Ctx): Fragment =
    val ast = app_t.app_t
    gen_and_add_dep_code(ast, parents, ctx)

    app_t.f match
      case tt : AST.BindT[?] =>
        mk_function_call(
          CGTree.RefExprT(tt),
          ast,
          Nil, // Note no generics for user-defined functions
          app_t.args,
          parents, ctx
        )
      case _ =>
        assert(false, "only named functions allowed")
  end visit_AppTData

  def visit_App0T[A](ast: AST.App0T[A], parents: ASTs, ctx: Ctx): Fragment =
    visit_AppTData(ast.as_AppTData, parents, ctx)
  end visit_App0T

  def visit_App1T[A, Z](ast: AST.App1T[A, Z], parents: ASTs, ctx: Ctx): Fragment =
    visit_AppTData(ast.as_AppTData, parents, ctx)
  end visit_App1T

  def visit_App2T[A, B, Z](ast: AST.App2T[A, B, Z], parents: ASTs, ctx: Ctx): Fragment =
    visit_AppTData(ast.as_AppTData, parents, ctx)
  end visit_App2T

  def visit_App3T[A, B, C, Z](ast: AST.App3T[A, B, C, Z], parents: ASTs, ctx: Ctx): Fragment =
    visit_AppTData(ast.as_AppTData, parents, ctx)
  end visit_App3T

  def visit_App4T[A, B, C, D, Z](ast: AST.App4T[A, B, C, D, Z], parents: ASTs, ctx: Ctx): Fragment =
    visit_AppTData(ast.as_AppTData, parents, ctx)
  end visit_App4T

  def visit_App5T[A, B, C, D, E, Z](ast: AST.App5T[A, B, C, D, E, Z], parents: ASTs, ctx: Ctx): Fragment =
    visit_AppTData(ast.as_AppTData, parents, ctx)
  end visit_App5T
  // end Functions

  //object Maps:
  def visit_MapNewT[K, V](ast: AST.MapNewT[K, V], parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    mk_primitive_call(
      PrimFun.Map_New,
      ast,
      List(ast.k_ty, ast.v_ty),
      Nil,
      parents, ctx
    )
  end visit_MapNewT

  def visit_MapGetT[K, V](ast: AST.MapGetT[K, V], parents: ASTs, ctx: Ctx): Fragment = ???
  def visit_MapPutT[K, V](ast: AST.MapPutT[K, V], parents: ASTs, ctx: Ctx): Fragment = ???

  def visit_MapValuesT[K, V](ast: AST.MapValuesT[K, V], parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    mk_primitive_call(
      PrimFun.Map_Values,
      ast,
      List(ast.k_ty, ast.v_ty),
      List(ast.map),
      parents, ctx
    )
  end visit_MapValuesT
  //end Maps

  //object Seqs:
  def visit_SeqNewT[K](ast: AST.SeqNewT[K], parents: ASTs, ctx: Ctx): Fragment =
    mk_primitive_call(
      PrimFun.Seq_New,
      ast,
      List(ast.item_ty),
      ast.items.toList,
      parents, ctx
    )
  end visit_SeqNewT

  def visit_SeqAddT[K](ast: AST.SeqAddT[K], parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    mk_primitive_call(
      PrimFun.Seq_Add,
      ast,
      List(ast.item_ty),
      List(ast.seq, ast.item),
      parents, ctx
    )
  end visit_SeqAddT

  def visit_SeqMapT[K, L](ast: AST.SeqMapT[K, L], parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    mk_primitive_call(
      PrimFun.Seq_Map,
      ast,
      List(ast.k_ty, ast.l_ty),
      List(ast.seq, ast.f),
      parents, ctx
    )
  end visit_SeqMapT

  def visit_SeqFoldLeftT[K, L](ast: AST.SeqFoldLeftT[K, L], parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    mk_primitive_call(
      PrimFun.Seq_Fold_Left,
      ast,
      List(ast.k_ty, ast.l_ty),
      List(ast.seq, ast.f),
      parents, ctx
    )
  end visit_SeqFoldLeftT
  //end Seqs

  //object Sets:
  def visit_SetNewT[K](ast: AST.SetNewT[K], parents: ASTs, ctx: Ctx): Fragment =
    mk_primitive_call(
      PrimFun.Set_New,
      ast,
      List(ast.item_ty),
      Nil,
      parents, ctx
    )
  end visit_SetNewT

  def visit_SetAddT[K](ast: AST.SetAddT[K], parents: ASTs, ctx: Ctx): Fragment = ???
  //end Sets

  //object OtherData:
  def visit_NoneT[T](ast: AST.NoneT[T], parents: ASTs, ctx: Ctx): Fragment = ???
  def visit_SomeT[T](ast: AST.SomeT[T], parents: ASTs, ctx: Ctx): Fragment = ???

  def visit_PairNewT[A, B](ast: AST.PairNewT[A, B], parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    mk_primitive_call(
      PrimFun.Pair_New,
      ast,
      List(ast.a.ty, ast.b.ty),
      List(ast.a, ast.b),
      parents, ctx
    )
  end visit_PairNewT

  def visit_PairFirstT[A, B](ast: AST.PairFirstT[A, B], parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    mk_primitive_call(
      PrimFun.Pair_First,
      ast,
      List(ast.a_ty, ast.b_ty),
      List(ast.pair),
      parents, ctx
    )
  end visit_PairFirstT

  def visit_PairSecondT[A, B](ast: AST.PairSecondT[A, B], parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    mk_primitive_call(
      PrimFun.Pair_Second,
      ast,
      List(ast.a_ty, ast.b_ty),
      List(ast.pair),
      parents, ctx
    )
  end visit_PairSecondT
  //end OtherData

  // object Structs:
  def visit_StructNewT(ast: AST.StructNewT[?, ?, ?], parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, parents, ctx)

    val field_names = CodeGenHelpers.struct_field_names(ast.struct_def)
    val params_ASTs = ast.params.toList.asInstanceOf[ASTs]

    require(field_names.length == params_ASTs.length)

    val params_cgtrees = for param <- params_ASTs yield visit(param, ast :: parents, ctx)

    val named_params = for (name, value) <- field_names.zip(params_cgtrees) yield NameCGTree(name, value)

    CGTree.StructNewExprT(
      ast.struct_def.name,
      named_params
    )
  end visit_StructNewT

  def visit_StructDotT(ast: AST.StructDotT[?, ?, ?], parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast.struct, ast :: parents, ctx)

    val prefix = visit(ast.struct, ast :: parents, ctx)

    val field_name  = ast.field_def.field.name
    val field_ty = ast.field_def.field.ty
    val field = NameType(field_name, field_ty)

    CGTree.DotExprT(prefix, field)
  end visit_StructDotT
  //end Structs

  //object Misc:
  def visit_TodoT(ast: AST.TodoT[?], parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast.ast, ast :: parents, ctx)

    ctx.add_tree(CGTree.CommentT("TODO " + ast.todo))
    visit(ast.ast, ast :: parents, ctx)
  end visit_TodoT
  //end Misc

  //object Props:
  def visit_PropT(ast: AST.PropT, parents: ASTs, ctx: Ctx): Fragment =
    gen_and_add_dep_code(ast, ast :: parents, ctx)

    ast.prop match
      case Prop.BoolP(predicate) =>
        val cg_predicate = visit(predicate, ast :: parents, ctx)
        cg_predicate
      case Prop.ForAllInRangeP(name, ty, bounds, upper_bound_is, predicate) =>
        // TODO implement
        ???
    end match
  end visit_PropT
end ASTToCGTree
