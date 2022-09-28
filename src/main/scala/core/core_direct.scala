package core

import trees.*
import trees.ast.*

import scala.annotation.targetName

type Id[T] = T

// Direct interpretation for Core
trait DirectCore extends Core[Id]:
  final type Bind[T] = Id[T]

  import TypeInference.given

  def str(s: String) = s
  def str_len(s: String): Nat = Nat(s.length)
  def str_char_at(s: String, i: Nat): Char = s.charAt(i.v.intValue)
  def str_concat(a: String, b: String): String = a + b

  def name(s: String) = s

  def char(c: Char): Char = c
  def char_to_str(c: Char): String = c.toString
  def char_eq(a: Char, b: Char): Bool = a == b

  def option_none[T: Ty]() = scala.None
  def option_some[T: Ty](value: T) = scala.Option(value)

  def bool(b: Bool): Bool = b
  def bool_or (a: Bool, b: Bool): Bool = a || b
  def bool_and(a: Bool, b: Bool): Bool = a && b
  def bool_not(a: Bool): Bool = ! a

  def nat(x: scala.Int): Nat =
    scala.Predef.require(x >= 0)
    Nat(x)
  end nat
  
  def nat_eq  (a: Nat, b: Nat): Bool = a eq_op b
  def nat_lt  (a: Nat, b: Nat): Bool = a lt_op b
  def nat_gteq(a: Nat, b: Nat): Bool = a gteq_op b

  def nat_add(a: Nat, b: Nat): Nat = a add_op b
  def nat_sub(a: Nat, b: Nat): Nat = a sub_op b
  def nat_mul(a: Nat, b: Nat): Nat = a mul_op b
  def nat_div(a: Nat, b: Nat): Nat = a div_op b

  def zat(x: scala.Int): Zat = Zat(x)

  def zat_of_nat(nat: Nat): Zat = nat.to_Zat
  def zat_mul(a: Zat, b: Zat): Zat = a mul_op b
  def zat_add(a: Zat, b: Zat): Zat = a add_op b

  def bind[T: Ty](name: Name, v: T) = v
  
  // TODO actually use the precondition
  def bind_add_precond3[T: Ty, A, B, C, Z](
    bind: T, 
    cond: Fun3[Id, A, B, C, Prop[Id]]
  )(implicit ev: T =:= Fun3[Id, A, B, C, Z]): T = 
  
    // TODO Use the pre-condition (maybe via function composition or OOP?)
    bind
  end bind_add_precond3

  def bind_add_postcond3[T: Ty, A, B, C, Z, R](
    bind: T,
    cond: Fun4[Id, A, B, C, R, Prop[Id]]
  )(implicit ev: T =:= Fun3[Id, A, B, C, Z]): T =

    // TODO Use the post-condition (maybe via function composition or OOP?)
    bind
  end bind_add_postcond3
  
  def _let[A: Ty, Z: Ty](bind: Bind[A], in: Fun1[Id, A, Z]): Id[Z] = in(bind)

  def ifte[T: Ty](cond: Bool, t: => T, e: => T): Id[T] = if cond then t else e

  def fun0[Z: Ty](f: Fun0[Id, Z]): Fun0[Id, Z] = f
  def app0[Z: Ty](f: Fun0[Id, Z]): Z = f()

  def fun1[A: Ty, Z: Ty](a: Name, f: Fun1[Id, A, Z]): Fun1[Id, A, Z] = f
  def app1[A: Ty, Z: Ty](f: A => Z, a: A): Z = f(a)

  def fun2[A: Ty, B: Ty, Z: Ty](a: Name, b: Name, f: Fun2[Id, A, B, Z]): Fun2[Id, A, B, Z] = f
  def app2[A: Ty, B: Ty, Z: Ty](f: Fun2[Id, A, B, Z], a: A, b: B): Z = f(a, b)

  def fun3[A: Ty, B: Ty, C: Ty, Z: Ty](a: Name, b: Name, c: Name, f: Fun3[Id, A, B, C, Z]): Fun3[Id, A, B, C, Z] = f
  def app3[A: Ty, B: Ty, C: Ty, Z: Ty](f: Fun3[Id, A, B, C, Z], a: A, b: B, c: C): Z = f(a, b, c)

  def fun4[A: Ty, B: Ty, C: Ty, D: Ty, Z: Ty](a: Name, b: Name, c: Name, d: Name, f: Fun4[Id, A, B, C, D, Z]): Fun4[Id, A, B, C, D, Z] = f
  def app4[A: Ty, B: Ty, C: Ty, D: Ty, Z: Ty](f: Fun4[Id, A, B, C, D, Z], a: A, b: B, c: C, d: D): Z = f(a, b, c, d)

  def fun5[A: Ty, B: Ty, C: Ty, D: Ty, E: Ty, Z: Ty](a: Name, b: Name, c: Name, d: Name, e: Name, f: Fun5[Id, A, B, C, D, E, Z]): Fun5[Id, A, B, C, D, E, Z] = f
  def app5[A: Ty, B: Ty, C: Ty, D: Ty, E: Ty, Z: Ty](f: Fun5[Id, A, B, C, D, E, Z], a: A, b: B, c: C, d: D, e: E): Z = f(a, b, c, d, e)


  def pair_new[A: Ty, B: Ty](a: A, b: B) = (a, b)
  def pair_first[A: Ty, B: Ty] (pair: Pair[A, B]): A = pair._1
  def pair_second[A: Ty, B: Ty](pair: Pair[A, B]): B = pair._2

  def map_new[K: Ty, V: Ty](): Map[K, V] = Map()
  def map_get[K: Ty, V: Ty](map: Map[K, V], key: K): V = map(key)
  def map_values[K: Ty, V: Ty](map: Map[K, V]): Seq[V] = map.values.toIndexedSeq
  def map_put[K: Ty, V: Ty](map: Map[K, V], key: K, value: V): Map[K, V] = map.updated(key, value)

  def set_new[K: Ty](): Set[K] = Set()
  def set_add[K: Ty](set: Set[K], k: K): Set[K] = set + k

  def seq_new[K: Ty](elems: K*): Seq[K] = Seq(elems*)
  def seq_add[K: Ty](seq: Seq[K], elem: K): Seq[K] = seq appended elem
  def seq_map[K: Ty, L: Ty](seq: Seq[K], f: K => L): Seq[L] = seq map f

  def seq_fold_left[K: Ty, L: Ty](seq: Seq[K], initial: L, f: Fun2[Id, L, K, L]): L =
    seq.foldLeft[L](initial)(f(_, _))
  end seq_fold_left

  def struct_new[Struct <: AnyStruct : Ty, Fields <: Tuple, Args <: Tuple](
    struct_def: StructDef[Struct, Fields, Args],
    args: Args
  ): Struct =
    struct_def.constr(args)
  end struct_new

  def dot[Struct <: AnyStruct: Ty, N <: FieldName, F: Ty](
    struct: Struct,
    field_def: FieldDef[Struct, N, F, Id]
  ): F =
    field_def.getter(struct)
  end dot

  def prop(p: Prop[Id]): Prop[Id] = p

  def Todo[T: Ty](todo: String, value: Id[T]): Id[T] = value
end DirectCore
