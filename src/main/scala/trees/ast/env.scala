package trees
package ast

import core.*

import scala.Option
import scala.annotation.tailrec

trait GlobalScopeMarker

object RootScopeMarker extends GlobalScopeMarker

enum Scope:
  case GlobalScope(marker: GlobalScopeMarker)
  case FunTScope(fun_t: FunT, bind_t: Option[AST.BindT[?]])
  case BindTScope(bind_t: AST.BindT[?])
  
  def debug_name: String =
    this match
      case `RootScope` => s"ROOT"
      case Scope.GlobalScope(marker) => s"Global(${ marker.getClass.getSimpleName })"
      case Scope.FunTScope(fun_t, Some(bind_t)) => s"FunT(${ bind_t.name }/${ bind_t.value.getClass.getSimpleName })"
      case Scope.FunTScope(fun_t, None) => s"FunT(${ fun_t })"
      case Scope.BindTScope(bind_t) => s"${ bind_t.name }"
    end match
  end debug_name
end Scope

val RootScope = Scope.GlobalScope(RootScopeMarker)

// Alternative to `scala.collection.mutable.LinkedHashSet`, using immutable data structures under the hood.
// Not thread-safe.
class LinkedSet[T] private(_vector: Vector[T], _set: Set[T]):
  private var vector = _vector // provides the order
  private var set    = _set

  def this() = this(Vector[T](), Set[T]())

  def add(item: T): Boolean =
    if set.contains(item) then return false
    set = set.incl(item)
    vector = vector.appended(item)

    true
  end add

  def add_all(items: List[T]): Unit =
    for item <- items do
      add(item)
    end for
  end add_all

  def remove(item: T): Boolean =
    if ! set.contains(item) then return false
    set = set.excl(item)
    vector = vector.drop(vector.indexOf(item))

    true
  end remove

  def contains(item: T): Boolean = set.contains(item)

  def toList: List[T] = vector.toList

  // Shallow copy. Due to the underlying immutable data structures, this one and the new LinkedSet
  // can be independently mutated.
  def copy(): LinkedSet[T] = LinkedSet(vector, set)
end LinkedSet

// Environment for resolving bindings while processing the AST
class Env(val owner: Scope, val parent: Option[Env], _bindings: LinkedSet[AST.BindT[?]] = LinkedSet()):
  // The bindings local to this scope
  private val bindings = _bindings

  // potentially recursive bindings, so basically the functions themselves.
  private val recursive_bindings = LinkedSet[AST.BindT[?]]()

  // for debugging
  override def toString: String =
    val __owners = owner_names.mkString("[", ", ", "]")
    val __my_bindings = my_bindings.map(_.to_short_str).mkString("[", ", ", "]")
    val __parent_bindings = all_parent_bindings.map(_.to_short_str).mkString("[", ", ", "]")

    s"Env{owners: ${__owners}, parent_binds: ${__parent_bindings}, ** my_binds: ${__my_bindings}}"
  end toString

  // copy is relatively cheap, since the underlying data structures are immutable.
  def copy(): Env = new Env(owner, parent.map(_.copy()), bindings.copy())

  // for debugging
  def owners: List[Scope] =
    @tailrec
    def get_them(scope: Env, acc: List[Scope]): List[Scope] =
      scope.parent match
        case Some(value) =>
          get_them(value, scope.owner :: acc)
        case None =>
          scope.owner :: acc
      end match
    end get_them

    get_them(this, Nil)
  end owners

  // for debugging
  def owner_names: List[String] = owners.map(_.debug_name)

  def contains(bind_t: AST.BindT[?]): Boolean =
    bindings.contains(bind_t) || parent.exists(_.contains(bind_t))
  end contains

  def add(bind_t: AST.BindT[?]): this.type =
    bindings.add(bind_t)
    this
  end add

  // add a potentially recursive binding
  def add_recursive(bind_t: AST.BindT[?]): Unit =
    recursive_bindings.add(bind_t)
  end add_recursive

  def contains_recursive(bind_t: AST.BindT[?]): Boolean =
    recursive_bindings.contains(bind_t) || parent.exists(_.contains_recursive(bind_t))
  end contains_recursive

  def add_all(bind_ts: BindTs): this.type =
    for bind_t <- bind_ts do
      add(bind_t)
    end for

    this
  end add_all

  // Removes the binding, first searching locally and, if not there, then moving up the parent chain.
  def remove(bind_t: AST.BindT[?]): Unit =
    if !bindings.remove(bind_t) then
      for p <- parent do
        p.remove(bind_t)
      end for
    end if
  end remove

  def new_child_env(owner: Scope): Env = Env(owner, Some(this))

  def new_child_env_for_BindT(bind_t: AST.BindT[?]): Env = new_child_env(Scope.BindTScope(bind_t))

  def my_bindings: BindTs = bindings.toList

  def all_bindings: BindTs =
    def add_bindinds_from_parent_first(env: Env, acc: LinkedSet[AST.BindT[?]]): Unit =
      for p <- env.parent do
        add_bindinds_from_parent_first(p, acc)
      end for

      acc.add_all(env.my_bindings)
    end add_bindinds_from_parent_first

    val acc = LinkedSet[AST.BindT[?]]()
    add_bindinds_from_parent_first(this, acc)

    acc.toList
  end all_bindings

  def all_parent_bindings: BindTs = parent.map(_.all_bindings).getOrElse(Nil)
end Env

def RootEnv(): Env = Env(RootScope, None)
