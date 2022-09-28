package trees

import core.*
import trees.ast.*
import trees.cgtree.*

class ASTToCGTreeCtx(env: Env, logger: Logger):
  private val trees = CGTreeBuffer()

  def do_with_new_FunT_ctx[T, C](fun_t: FunT, bind_t: Option[AST.BindT[T]])(closure: ASTToCGTreeCtx => C): C =
    val new_owner = Scope.FunTScope(fun_t, bind_t)
    val new_scope = env.new_child_env(new_owner)
    val new_ctx = ASTToCGTreeCtx(new_scope, logger)

    closure(new_ctx)
  end do_with_new_FunT_ctx

  def scope_owner: Scope = env.owner

  def new_with_env_copy() = ASTToCGTreeCtx(env.copy(), logger)

  def contains[T](bind_t: AST.BindT[T]): Boolean = env.contains(bind_t)

  def get_my_trees(): CGTrees = trees.get_trees()

  def add_tree(t: CGTree): Unit =
    logger.log(s"** Add tree: $t")
    logger.log(s"   for env : $env")
    trees.add_tree(t)
  end add_tree

  def add_trees(ts: CGTrees): Unit =
    for t <- ts do
      add_tree(t)
    end for
  end add_trees

  def get_my_bindings(): BindTs = env.my_bindings

  def add_binding[T](bind_t: AST.BindT[T]): Unit =
    logger.log(s"++ Add bind_t: $bind_t")
    logger.log(s"++   to scope: $scope_owner")
    env.add(bind_t)
  end add_binding

  def add_bindings(bind_ts: BindTs): Unit =
    for bind_t <- bind_ts do
      add_binding(bind_t)
    end for
  end add_bindings

  def get_all_bindings(): BindTs = env.all_bindings

  def add_bindings_of(other: ASTToCGTreeCtx): Unit =
    this.add_bindings(other.get_my_bindings())
  end add_bindings_of

  // for debugging
  override def toString: String = s"Ctx($env)"
end ASTToCGTreeCtx
