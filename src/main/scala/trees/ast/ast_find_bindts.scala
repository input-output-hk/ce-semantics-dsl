package trees
package ast

import core.*

class FindBindTs[T](
  ast: AST[T],
  logger: Logger = Logger()
) extends ASTVisitor[T, Env, Unit, BindTs](
  ast,
  logger
):
  type Ctx = Env

  protected val initial_ctx: Env = RootEnv()

  def get_initial_ctx(): Ctx = initial_ctx

  def get_result(ast: AST[T], vr: Unit, ctx: Ctx): BindTs =
    initial_ctx.my_bindings
  end get_result

  //object Numbers:
  def visit_NatT(ast: AST.NatT, parents: ASTs, ctx: Ctx): Unit = {}

  def visit_ZatT(ast: AST.ZatT, parents: ASTs, ctx: Ctx): Unit = {}

  def visit_ZatOfNatT(ast: AST.ZatOfNatT, parents: ASTs, ctx: Ctx): Unit =
    visit(ast.nat, ast :: parents, ctx)
  end visit_ZatOfNatT

  def visit_ArithBinOpT[T](ast: AST.ArithBinOpT[T], parents: ASTs, ctx: Ctx): Unit =
    visit(ast.a, ast :: parents, ctx)
    visit(ast.b, ast :: parents, ctx)
  end visit_ArithBinOpT
  //end Numbers

  //object Bools:
  def visit_BoolT(ast: AST.BoolT, parents: ASTs, ctx: Ctx): Unit = {}

  def visit_BoolBinOpT(ast: AST.BoolBinOpT, parents: ASTs, ctx: Ctx): Unit =
    visit(ast.a, ast :: parents, ctx)
    visit(ast.b, ast :: parents, ctx)
  end visit_BoolBinOpT

  def visit_BoolNotT(ast: AST.BoolNotT, parents: ASTs, ctx: Ctx): Unit =
    visit(ast.a, ast :: parents, ctx)
  end visit_BoolNotT

  def visit_ComparisonBinOpT[T](ast: AST.ComparisonBinOpT[T], parents: ASTs, ctx: Ctx): Unit =
    visit(ast.a, ast :: parents, ctx)
    visit(ast.b, ast :: parents, ctx)
  end visit_ComparisonBinOpT
  //end Bools

  //object Strings:
  def visit_StringT(ast: AST.StringT, parents: ASTs, ctx: Ctx): Unit = {}

  def visit_StringLenT(ast: AST.StringLenT, parents: ASTs, ctx: Ctx): Unit =
    visit(ast.str, ast :: parents, ctx)
  end visit_StringLenT

  def visit_StringCharAtT(ast: AST.StringCharAtT, parents: ASTs, ctx: Ctx): Unit =
    visit(ast.str, ast :: parents, ctx)
    visit(ast.index, ast :: parents, ctx)
  end visit_StringCharAtT

  def visit_StringConcatT(ast: AST.StringConcatT, parents: ASTs, ctx: Ctx): Unit =
    visit(ast.a, ast :: parents, ctx)
    visit(ast.b, ast :: parents, ctx)
  end visit_StringConcatT

  def visit_CharT(ast: AST.CharT, parents: ASTs, ctx: Ctx): Unit = {}

  def visit_CharToStrT(ast: AST.CharToStrT, parents: ASTs, ctx: Ctx): Unit =
    visit(ast.c, ast :: parents, ctx)
  end visit_CharToStrT

  def visit_CharEqT(ast: AST.CharEqT, parents: ASTs, ctx: Ctx): Unit =
    visit(ast.a, ast :: parents, ctx)
    visit(ast.b, ast :: parents, ctx)
  end visit_CharEqT
  //end Strings

  //object Binds:
  def visit_ParamT[T](ast: AST.ParamT[T], parents: ASTs, ctx: Ctx): Unit = {}

  def visit_BindT[T](bind_t: AST.BindT[T], parents: ASTs, ctx: Ctx) =
    // look into the definition only if this is an unknown binding
    if ! (ctx.contains(bind_t) || ctx.contains_recursive(bind_t)) then
      val new_ctx = ctx.new_child_env(Scope.BindTScope(bind_t))
      log(s"Created new ctx for ${bind_t}")

      // OK, now we have a problem if the bind_t is recursive.
      // So we tentative add just that into the scope and move on.
      new_ctx.add_recursive(bind_t)

      log(s"  ... visit the value of ${bind_t.name} = ${bind_t.value}")
      visit(bind_t.value, bind_t :: parents, new_ctx)

      // Note that if we were careful about how we visited the tree up to here,
      //      then dependencies will be topologically sorted
      ctx.add_all(new_ctx.my_bindings)
    else
      log(s"  Is already defined: ${bind_t}")
    end if

    // This is the right spot to add bind_t only if bind_t is non-recursive.
    // Check above, where we add it before the (potentially recursive) visit.
    ctx.add(bind_t)
  end visit_BindT

  def visit_LetT[A, Z](ast: AST.LetT[A, Z], parents: ASTs, ctx: Ctx): Unit =
    // TODO ?
    ()
  end visit_LetT
  
  //end Binds

  //object Controls:
  def visit_IfThenElseT[T](ast: AST.IfThenElseT[T], parents: ASTs, ctx: Ctx): Unit =
    visit(ast._if,   ast :: parents, ctx)
    visit(ast._then, ast :: parents, ctx)
    visit(ast._else, ast :: parents, ctx)
  end visit_IfThenElseT

  def visit_ForLoopT(ast: AST.ForLoopT[?], parents: ASTs, ctx: Ctx): Unit =
    visit(ast.lower, ast :: parents, ctx)
    visit(ast.upper, ast :: parents, ctx)
    visit(ast.body, ast :: parents, ctx)
  end visit_ForLoopT
  //end Controls


  //object Functions:
  def visit_AppTData[A](app_t_data: AppTData, parents: ASTs, ctx: Ctx): Unit =
    val ast = app_t_data.app_t

    // first discover for the argument(s)
    for arg <- app_t_data.args do
      visit(arg, ast :: parents, ctx)
    end for

    // then for the function itself
    visit(app_t_data.f, ast :: parents, ctx)
  end visit_AppTData

  def visit_Fun0T[Z](ast: AST.Fun0T[Z], parents: ASTs, ctx: Ctx): Unit =
    // TODO new scope ??
    // Note beware that a FunXT embedded in a BindT already has new scope, so may check `parents`
    //      before deciding to create a new scope here.
    val f_applied = ast.f()
    visit(/*ast.*/f_applied, ast :: parents, ctx)
  end visit_Fun0T

  def visit_App0T[Z](ast: AST.App0T[Z], parents: ASTs, ctx: Ctx): Unit =
    visit_AppTData(ast.as_AppTData, parents, ctx)
  end visit_App0T

  def visit_Fun1T[A, Z](ast: AST.Fun1T[A, Z], parents: ASTs, ctx: Ctx): Unit =
    // TODO new scope ??
    // Note beware that a FunXT embedded in a BindT already has new scope, so may check `parents`
    //      before deciding to create a new scope here.
    val f_applied = ast.f(ast.a)
    visit(/*ast.*/f_applied, ast :: parents, ctx)
  end visit_Fun1T

  def visit_App1T[A, Z](ast: AST.App1T[A, Z], parents: ASTs, ctx: Ctx): Unit =
    visit_AppTData(ast.as_AppTData, parents, ctx)
  end visit_App1T

  def visit_Fun2T[A, B, Z](ast: AST.Fun2T[A, B, Z], parents: ASTs, ctx: Ctx): Unit =
    // TODO new scope ??
    // Note beware that a FunXT embedded in a BindT already has new scope, so may check `parents`
    //      before deciding to create a new scope here.
    val f_applied = ast.f(ast.a, ast.b)
    visit(/*ast.*/f_applied, ast :: parents, ctx)
  end visit_Fun2T

  def visit_App2T[A, B, Z](ast: AST.App2T[A, B, Z], parents: ASTs, ctx: Ctx): Unit =
    visit_AppTData(ast.as_AppTData, parents, ctx)
  end visit_App2T

  def visit_Fun3T[A, B, C, Z](ast: AST.Fun3T[A, B, C, Z], parents: ASTs, ctx: Ctx): Unit =
    // TODO new scope ??
    // Note beware that a FunXT embedded in a BindT already has new scope, so may check `parents`
    //      before deciding to create a new scope here.
    val f_applied = ast.f(ast.a, ast.b, ast.c)
    visit(/*ast.*/f_applied, ast :: parents, ctx)
  end visit_Fun3T

  def visit_App3T[A, B, C, Z](ast: AST.App3T[A, B, C, Z], parents: ASTs, ctx: Ctx): Unit =
    visit_AppTData(ast.as_AppTData, parents, ctx)
  end visit_App3T

  def visit_Fun4T[A, B, C, D, Z](ast: AST.Fun4T[A, B, C, D, Z], parents: ASTs, ctx: Ctx): Unit =
    // TODO new scope ??
    // Note beware that a FunXT embedded in a BindT already has new scope, so may check `parents`
    //      before deciding to create a new scope here.
    val f_applied = ast.f(ast.a, ast.b, ast.c, ast.d)
    visit(/*ast.*/f_applied, ast :: parents, ctx)
  end visit_Fun4T

  def visit_App4T[A, B, C, D, Z](ast: AST.App4T[A, B, C, D, Z], parents: ASTs, ctx: Ctx): Unit =
    visit_AppTData(ast.as_AppTData, parents, ctx)
  end visit_App4T

  def visit_Fun5T[A, B, C, D, E, Z](ast: AST.Fun5T[A, B, C, D, E, Z], parents: ASTs, ctx: Ctx): Unit =
    // TODO new scope ??
    // Note beware that a FunXT embedded in a BindT already has new scope, so may check `parents`
    //      before deciding to create a new scope here.
    val f_applied = ast.f(ast.a, ast.b, ast.c, ast.d, ast.e)
    visit(/*ast.*/f_applied, ast :: parents, ctx)
  end visit_Fun5T

  def visit_App5T[A, B, C, D, E, Z](ast: AST.App5T[A, B, C, D, E, Z], parents: ASTs, ctx: Ctx): Unit =
    visit_AppTData(ast.as_AppTData, parents, ctx)
  end visit_App5T
  //end Functions

  //object OtherData:
  def visit_NoneT[T](ast: AST.NoneT[T], parents: ASTs, ctx: Ctx): Unit = {}

  def visit_SomeT[T](ast: AST.SomeT[T], parents: ASTs, ctx: Ctx): Unit =
    visit(ast.some, ast :: parents, ctx)
  end visit_SomeT

  def visit_PairNewT[A, B](ast: AST.PairNewT[A, B], parents: ASTs, ctx: Ctx): Unit =
    visit(ast.a, ast :: parents, ctx)
    visit(ast.b, ast :: parents, ctx)
  end visit_PairNewT

  def visit_PairFirstT[A, B](ast: AST.PairFirstT[A, B], parents: ASTs, ctx: Ctx): Unit =
    visit(ast.pair, ast :: parents, ctx)
  end visit_PairFirstT

  def visit_PairSecondT[A, B](ast: AST.PairSecondT[A, B], parents: ASTs, ctx: Ctx): Unit =
    visit(ast.pair, ast :: parents, ctx)
  end visit_PairSecondT
  //end OtherData

  //object Maps:
  def visit_MapNewT[K, V](ast: AST.MapNewT[K, V], parents: ASTs, ctx: Ctx): Unit = {}

  def visit_MapGetT[K, V](ast: AST.MapGetT[K, V], parents: ASTs, ctx: Ctx): Unit =
    visit(ast.map, ast :: parents, ctx)
    visit(ast.key, ast :: parents, ctx)
  end visit_MapGetT

  def visit_MapPutT[K, V](ast: AST.MapPutT[K, V], parents: ASTs, ctx: Ctx): Unit =
    visit(ast.map, ast :: parents, ctx)
    visit(ast.key, ast :: parents, ctx)
    visit(ast.value, ast :: parents, ctx)
  end visit_MapPutT

  def visit_MapValuesT[K, V](ast: AST.MapValuesT[K, V], parents: ASTs, ctx: Ctx): Unit =
    visit(ast.map, ast :: parents, ctx)
  end visit_MapValuesT
  //end Maps

  //object Seqs:
  def visit_SeqNewT[K](ast: AST.SeqNewT[K], parents: ASTs, ctx: Ctx): Unit =
    for item <- ast.items do
      visit(item, ast :: parents, ctx)
    end for
  end visit_SeqNewT

  def visit_SeqAddT[K](ast: AST.SeqAddT[K], parents: ASTs, ctx: Ctx): Unit =
    visit(ast.seq,  ast :: parents, ctx)
    visit(ast.item, ast :: parents, ctx)
  end visit_SeqAddT

  def visit_SeqMapT[K, L](ast: AST.SeqMapT[K, L], parents: ASTs, ctx: Ctx): Unit =
    visit(ast.seq,  ast :: parents, ctx)
    visit(ast.f,  ast :: parents, ctx)
  end visit_SeqMapT

  def visit_SeqFoldLeftT[K, L](ast: AST.SeqFoldLeftT[K, L], parents: ASTs, ctx: Ctx): Unit =
    visit(ast.seq, ast :: parents, ctx)
    visit(ast.initial, ast :: parents, ctx)
    visit(ast.f, ast :: parents, ctx)
  end visit_SeqFoldLeftT
  //end Seqs

  //object Sets:
  def visit_SetNewT[K](ast: AST.SetNewT[K], parents: ASTs, ctx: Ctx): Unit = {}

  def visit_SetAddT[K](ast: AST.SetAddT[K], parents: ASTs, ctx: Ctx): Unit =
    visit(ast.set,  ast :: parents, ctx)
    visit(ast.item, ast :: parents, ctx)
  end visit_SetAddT
  //end Sets

  //object Structs:
  def visit_StructNewT(ast: AST.StructNewT[?, ?, ?], parents: ASTs, ctx: Ctx): Unit =
    val param_list = ast.params.toList

    // Note on the use of `case`: ideally we do not want to filter the params but require they all have type
    //      AST[_].
    for case param: AST[_] <- param_list do
      visit(param, ast :: parents, ctx)
    end for
  end visit_StructNewT

  def visit_StructDotT(ast: AST.StructDotT[?, ?, ?], parents: ASTs, ctx: Ctx): Unit =
    visit(ast.struct, ast :: parents, ctx)
  end visit_StructDotT
  //end Structs

  //object Misc:
  def visit_TodoT(ast: AST.TodoT[?], parents: ASTs, ctx: Ctx): Unit =
    visit(ast.ast, ast :: parents, ctx)
  end visit_TodoT
  //end Misc

  //object Props:
  def visit_PropT(ast: AST.PropT, parents: ASTs, ctx: Ctx): Unit =
    ast.prop match
      case Prop.BoolP(expr) =>
        visit(expr, ast :: parents, ctx)
      case Prop.ForAllInRangeP(name, ty, (lower, upper), u_bound_is, predicate) =>
        visit(lower, ast :: parents, ctx)
        visit(upper, ast :: parents, ctx)
        visit(predicate, ast :: parents, ctx)
  end visit_PropT
  //end Props

  override def visit(ast: AST[?], parents: ASTs, ctx: Ctx): Unit =
    ast match
      // optimize away some common cases, basically to unclutter logging while debugging :)
      case AST.ParamT(name, ty) =>
      case AST.StringT(str) =>
      case AST.CharT(c) =>
      case AST.NoneT(ty) =>
      case AST.BoolT(bool) =>
      case AST.NatT(nat) =>
      case AST.ZatT(zat) =>
      case _ => super.visit(ast, parents, ctx)
  end visit

end FindBindTs

@main def find_binds_demo: Unit =
  def run[T](ast: AST[T], max_level: Int = 1024): Unit =
    println("=========")

    val finder = FindBindTs(ast).set_logging_off()
    val bindings = finder.run()

    def show[T](ast: AST[_]): Unit =
      /*pprint.p*/println(ast.to_short_str)
      //if ast.isInstanceOf[BindT[_]] then
      //  val bind_t = ast.asInstanceOf[BindT[_]]
      //  print(s"BindT(${bind_t.ident}, ${bind_t.definition.getClass.getSimpleName})")
      //else if ast.isInstanceOf[AppT[_, _]] then
      //  val app_t = ast.asInstanceOf[AppT[_, _]]
      //  print("AppT(")
      //  show(app_t.fun)
      //  print(", ")
      //  show(app_t.x)
      //  printf(")")
      //else
      //  print(ast)
      //end if
    end show

    print("level = ")
    /*pprint.p*/println(max_level)
    print("ast = ")
    /*pprint.p*/println(ast/*, width = 350*/)

    print("for ")
    show(ast)
    println()
    print("==>")
    print(s" deps [N=${bindings.length}] = ")
    /*pprint.p*/println(bindings.map(b => s"${ b.name } = ${ b.value.getClass.getSimpleName }: ${b.ty.untag.repr}"))
    //print(s"deps [L=${level}, N=${bindings.length}] = ")
    //pprint.pprintln(bindings)
  end run

  import chain.demo.ASTChainWithDemo._
  //run(ASTDemo.One, 1)
  //run(ASTDemo.add, 2)
  //run(add_1(Zero), 1)
  //run(ASTDemo.N_One)
  //run(ASTDemo.One_plus_Zero, 2)
  //run(ASTDemo.foobar, 1)
  //run(ASTDemo.foobar2, 2)
  run(foobar3)
  //run(ASTDemo.log)
  //run(ASTDemo.kes_period_of_n)
  //run(ASTDemo.txin_new)
  //run(ASTDemo.utxo_new_empty)
  //run(ASTDemo.txttl)
  //run(ASTDemo.ppupdateenv_new)
  //run(ASTDemo.min_fee)
end find_binds_demo


