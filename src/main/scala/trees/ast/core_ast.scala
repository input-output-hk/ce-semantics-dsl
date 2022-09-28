package trees
package ast

import core.*
import trees.ast.AST.*
import trees.ArithBinOp.*
import trees.ComparisonBinOp.*
import trees.cgtree.{CGTreeBaseCtx, CGTreeBuffer, CGTreeToScala, CGTreeVisitorBase, CGTrees}

import scala.collection.mutable.LinkedHashSet

// AST interpretation for Core.
trait ASTCore extends Core[AST]:
  final type Bind[T] = AST.BindT[T]

  import TypeInference.given

  def name(s: String) = s

  def str(s: String) = StringT(s)
  def str_len(s: AST[String]): AST[Nat] = StringLenT(s)
  def str_char_at(s: AST[String], i: AST[Nat]): AST[Char] = StringCharAtT(s, i)
  def str_concat(a: AST[String], b: AST[String]): AST[String] = StringConcatT(a, b)

  def char(c: Char): AST[Char] = CharT(c)
  def char_to_str(c: AST[Char]): AST[String] = CharToStrT(c)
  def char_eq(a: AST[Char], b: AST[Char]): AST[Bool] = CharEqT(a, b) // TODO Could use a binary op (?)

  def option_none[T: Ty](): AST[Option[AST[T]]] =
    val ty = summon[Ty[Option[AST[T]]]]

    NoneT[T](ty)
  end option_none

  def option_some[T: Ty](value: AST[T]): AST[Option[AST[T]]] =
    val ty = summon[Ty[Option[AST[T]]]]

    SomeT(value, ty)
  end option_some


  def bool(b: Bool): AST[Bool] = AST.BoolT(b)
  def bool_or (a: AST[Bool], b: AST[Bool]): AST[Bool] = BoolBinOpT(BoolBinOp.OrOp,  a, b)
  def bool_and(a: AST[Bool], b: AST[Bool]): AST[Bool] = BoolBinOpT(BoolBinOp.AndOp, a, b)
  def bool_not(a: AST[Bool]): AST[Bool] = BoolNotT(a)

  def nat(x: scala.Int): AST[Nat] =
    require(x >= 0)
    NatT(Nat(scala.BigInt(x)))
  end nat

  def nat_eq  (a: AST[Nat], b: AST[Nat]): AST[Bool] = ComparisonBinOpT(EqOp,   a, b)
  def nat_lt  (a: AST[Nat], b: AST[Nat]): AST[Bool] = ComparisonBinOpT(LtOp,   a, b)
  def nat_gteq(a: AST[Nat], b: AST[Nat]): AST[Bool] = ComparisonBinOpT(GtEqOp, a, b)

  def nat_add (a: AST[Nat], b: AST[Nat]): AST[Nat] = ArithBinOpT(AddOp, a, b)
  def nat_sub (a: AST[Nat], b: AST[Nat]): AST[Nat] = ArithBinOpT(SubOp, a, b)
  def nat_mul (a: AST[Nat], b: AST[Nat]): AST[Nat] = ArithBinOpT(MulOp, a, b)
  def nat_div (a: AST[Nat], b: AST[Nat]): AST[Nat] = ArithBinOpT(DivOp, a, b)

  def zat(x: scala.Int): AST[Zat] = ZatT(Zat(x))
  def zat_of_nat(nat: AST[Nat]): AST[Zat] = ZatOfNatT(nat)
  def zat_mul(a: AST[Zat], b: AST[Zat]): AST[Zat] = ArithBinOpT(MulOp, a, b)
  def zat_add(a: AST[Zat], b: AST[Zat]): AST[Zat] = ArithBinOpT(AddOp, a, b)

  def bind[T: Ty](name: Name, value: AST[T]): BindT[T] =
    val ty = summon[Ty[T]]
    require(ty == value.ty)

    val bind_t: BindT[T] = BindT(name, value, ty, BindTCreation.BindTeDSL)

    bind_t
  end bind

  def bind_add_precond3[T: Ty, A, B, C, Z](
    bind_t: AST.BindT[T],
    cond: AST[Fun3[AST, A, B, C, Prop[AST]]]
  )(implicit ev: T =:= Fun3[AST, A, B, C, Z]): AST.BindT[T] =
    val new_pre_cond = bind_t.pre_cond.appended(cond)

    bind_t.copy(pre_cond = new_pre_cond)
  end bind_add_precond3


  def bind_add_postcond3[T: Ty, A, B, C, Z, R](
    bind_t: AST.BindT[T],
    cond: AST[Fun4[AST, A, B, C, R, Prop[AST]]]
  )(implicit ev: T =:= Fun3[AST, A, B, C, Z]): AST.BindT[T] =
    val new_post_cond = bind_t.post_cond.appended(cond)

    bind_t.copy(post_cond = new_post_cond)
  end bind_add_postcond3

  def _let[A: Ty, Z: Ty](bind: AST.BindT[A], in: Fun1[AST, A, Z]): AST[Z] =
    val name = bind.name
    val value = bind.value
    
    val a_ty = summon[Ty[A]]
    val a: AST.ParamT[A] = AST.ParamT(name, a_ty)
    val body = in(a)
    
    val ty = summon[Ty[Z]]
    AST.LetT(name, value, in, body, a_ty, ty)
  end _let

  def ifte[T: Ty](i: AST[Bool], t: => AST[T], e: => AST[T]): AST[T] =
    val ty = summon[Ty[T]]

    IfThenElseT(i, t, e, ty)
  end ifte

  def fun0[Z: Ty](f: () => AST[Z]): AST[Fun0[AST, Z]] =
    val ty = summon[Ty[Fun0[AST, Z]]]

    Fun0T(() => f(), f, ty)
  end fun0

  def app0[Z: Ty](f: AST[Fun0[AST, Z]]): AST[Z] =
    val ty = summon[Ty[Z]]

    App0T(f, ty)
  end app0

  def fun1[A: Ty, Z: Ty](a_name: Name, f: Fun1[AST, A, Z]): AST[Fun1[AST, A, Z]] =
    val a_ty = summon[Ty[A]]
    val ty   = summon[Ty[Fun1[AST, A, Z]]]

    val a: ParamT[A] = ParamT[A](a_name, a_ty)
    val f_applied = () => f(a)

    Fun1T(a, f_applied, f, ty)
  end fun1

  def app1[A: Ty, Z: Ty](f: AST[Fun1[AST, A, Z]], a: AST[A]): AST[Z] =
    val ty = summon[Ty[Z]]

    App1T(f, a, ty)
  end app1

  def fun2[A: Ty, B: Ty, Z: Ty](
    a_name: Name,
    b_name: Name,
    f: Fun2[AST, A, B, Z]
  ): AST[Fun2[AST, A, B, Z]] =
    val a_ty = summon[Ty[A]]
    val b_ty = summon[Ty[B]]
    val ty = summon[Ty[Fun2[AST, A, B, Z]]]

    val a: ParamT[A] = ParamT[A](a_name, a_ty)
    val b: ParamT[B] = ParamT[B](b_name, b_ty)
    val f_applied = () => f(a, b)

    Fun2T(a, b, f_applied, f, ty)
  end fun2

  def app2[A: Ty, B: Ty, Z: Ty](
    f: AST[Fun2[AST, A, B, Z]],
    a: AST[A],
    b: AST[B]
  ): AST[Z] =
    val ty = summon[Ty[Z]]

    App2T(f, a, b, ty)
  end app2

  def fun3[A: Ty, B: Ty, C: Ty, Z: Ty](
    a_name: Name,
    b_name: Name,
    c_name: Name,
    f: Fun3[AST, A, B, C, Z]
  ): AST[Fun3[AST, A, B, C, Z]] =
    val a_ty = summon[Ty[A]]
    val b_ty = summon[Ty[B]]
    val c_ty = summon[Ty[C]]
    val ty = summon[Ty[Fun3[AST, A, B, C, Z]]]

    val a: ParamT[A] = ParamT[A](a_name, a_ty)
    val b: ParamT[B] = ParamT[B](b_name, b_ty)
    val c: ParamT[C] = ParamT[C](c_name, c_ty)
    val f_applied = () => f(a, b, c)

    Fun3T(a, b, c, f_applied, f, ty)
  end fun3

  def app3[A: Ty, B: Ty, C: Ty, Z: Ty](
    f: AST[Fun3[AST, A, B, C, Z]],
    a: AST[A],
    b: AST[B],
    c: AST[C]
  ): AST[Z] =
    val ty = summon[Ty[Z]]

    App3T(f, a, b, c, ty)
  end app3

  def fun4[A: Ty, B: Ty, C: Ty, D: Ty, Z: Ty](
    a_name: Name,
    b_name: Name,
    c_name: Name,
    d_name: Name,
    f: Fun4[AST, A, B, C, D, Z]
  ): AST[Fun4[AST, A, B, C, D, Z]] =
    val a_ty = summon[Ty[A]]
    val b_ty = summon[Ty[B]]
    val c_ty = summon[Ty[C]]
    val d_ty = summon[Ty[D]]
    val ty = summon[Ty[Fun4[AST, A, B, C, D, Z]]]

    val a: ParamT[A] = ParamT[A](a_name, a_ty)
    val b: ParamT[B] = ParamT[B](b_name, b_ty)
    val c: ParamT[C] = ParamT[C](c_name, c_ty)
    val d: ParamT[D] = ParamT[D](d_name, d_ty)
    val f_applied = () => f(a, b, c, d)

    Fun4T(a, b, c, d, f_applied, f, ty)
  end fun4

  def app4[A: Ty, B: Ty, C: Ty, D: Ty, Z: Ty](
    f: AST[Fun4[AST, A, B, C, D, Z]],
    a: AST[A],
    b: AST[B],
    c: AST[C],
    d: AST[D]
  ): AST[Z] =
    val ty = summon[Ty[Z]]

    App4T(f, a, b, c, d, ty)
  end app4

  def fun5[A: Ty, B: Ty, C: Ty, D: Ty, E: Ty, Z: Ty](
    a_name: Name,
    b_name: Name,
    c_name: Name,
    d_name: Name,
    e_name: Name,
    f: Fun5[AST, A, B, C, D, E, Z]
  ): AST[Fun5[AST, A, B, C, D, E, Z]] =
    val a_ty = summon[Ty[A]]
    val b_ty = summon[Ty[B]]
    val c_ty = summon[Ty[C]]
    val d_ty = summon[Ty[D]]
    val e_ty = summon[Ty[E]]
    val ty = summon[Ty[Fun5[AST, A, B, C, D, E, Z]]]

    val a: ParamT[A] = ParamT[A](a_name, a_ty)
    val b: ParamT[B] = ParamT[B](b_name, b_ty)
    val c: ParamT[C] = ParamT[C](c_name, c_ty)
    val d: ParamT[D] = ParamT[D](d_name, d_ty)
    val e: ParamT[E] = ParamT[E](e_name, e_ty)
    val f_applied = () => f(a, b, c, d, e)

    Fun5T(a, b, c, d, e, f_applied, f, ty)
  end fun5

  def app5[A: Ty, B: Ty, C: Ty, D: Ty, E: Ty, Z: Ty](
    f: AST[Fun5[AST, A, B, C, D, E, Z]],
    a: AST[A],
    b: AST[B],
    c: AST[C],
    d: AST[D],
    e: AST[E]
  ): AST[Z] =
    val ty = summon[Ty[Z]]

    App5T(f, a, b, c, d, e, ty)
  end app5
  
  def pair_new[A: Ty, B: Ty](a: AST[A], b: AST[B]) =
    val ty = summon[Ty[Pair[AST[A], AST[B]]]]

    PairNewT(a, b, ty)
  end pair_new

  def pair_first[A: Ty, B: Ty](pair: AST[Pair[AST[A], AST[B]]]) =
    val a_ty = summon[Ty[A]]
    val b_ty = summon[Ty[B]]

    PairFirstT(pair, a_ty, b_ty)
  end pair_first

  def pair_second[A: Ty, B: Ty](pair: AST[Pair[AST[A], AST[B]]]) =
    val a_ty = summon[Ty[A]]
    val b_ty = summon[Ty[B]]
    PairSecondT(pair, a_ty, b_ty)
  end pair_second

  def map_new[K: Ty, V: Ty](): AST[Map[AST[K], AST[V]]] =
    val k_ty = summon[Ty[K]]
    val v_ty = summon[Ty[V]]
    val ty = summon[Ty[Map[AST[K], AST[V]]]]
    MapNewT(k_ty, v_ty, ty)
  end map_new

  def map_get[K: Ty, V: Ty](map: AST[Map[AST[K], AST[V]]], key: AST[K]) =
    val ty = summon[Ty[V]]

    MapGetT(map, key, ty)
  end map_get

  def map_values[K: Ty, V: Ty](map: AST[Map[AST[K], AST[V]]]) =
    val k_ty = summon[Ty[K]]
    val v_ty = summon[Ty[V]]
    val ty = summon[Ty[Seq[AST[V]]]]

    MapValuesT(map, k_ty, v_ty, ty)
  end map_values

  def map_put[K: Ty, V: Ty](map: AST[Map[AST[K], AST[V]]], key: AST[K], value: AST[V]) =
    MapPutT(map, key, value)
  end map_put

  def set_new[K: Ty](): AST[Set[AST[K]]] =
    val item_ty = summon[Ty[K]]
    val ty = summon[Ty[Set[AST[K]]]]

    SetNewT(item_ty, ty)
  end set_new

  def set_add[K: Ty](set: AST[Set[AST[K]]], item: AST[K]): AST[Set[AST[K]]] =
    val ty = summon[Ty[Set[AST[K]]]]

    SetAddT(set, item, ty)
  end set_add

  def seq_new[K: Ty](elems: AST[K]*) =
    val item_ty = summon[Ty[K]]
    val ty = summon[Ty[Seq[AST[K]]]]

    SeqNewT(elems.toIndexedSeq, item_ty, ty)
  end seq_new

  def seq_add[K: Ty](seq: AST[Seq[AST[K]]], item: AST[K]) =
    val item_ty = summon[Ty[K]]

    SeqAddT(seq, item, item_ty)
  end seq_add

  def seq_map[K: Ty, L: Ty](seq: AST[Seq[AST[K]]], f: AST[AST[K] => AST[L]]) =
    val k_ty = summon[Ty[K]]
    val l_ty = summon[Ty[L]]
    val ty = summon[Ty[Seq[AST[L]]]]

    SeqMapT(seq, f, k_ty, l_ty, ty)
  end seq_map

  def seq_fold_left[K: Ty, L: Ty](seq: AST[Seq[AST[K]]], initial: AST[L], f: AST[Fun2[AST, L, K, L]]): AST[L] =
    val k_ty = summon[Ty[K]]
    val l_ty = summon[Ty[L]]

    SeqFoldLeftT(seq, initial, f, k_ty, l_ty)
  end seq_fold_left

  def struct_new[Struct <: AnyStruct: Ty, Fields <: Tuple, Args <: Tuple](
    struct_def: StructDef[Struct, Fields, Args], args: Args
  ): AST[Struct] =
    val ty = summon[Ty[Struct]]

    AST.StructNewT(struct_def, args, ty)
  end struct_new

  def dot[Struct <: AnyStruct: Ty, N <: FieldName, F: Ty](
    struct: AST[Struct],
    field_def: FieldDef[Struct, N, F, AST]
  ): AST[F] =
    val ty = summon[Ty[F]]

    AST.StructDotT(struct, field_def, ty)
  end dot

  def Todo[T: Ty](todo: String, value: AST[T]): AST[T] =
    val ty = summon[Ty[T]]

    AST.TodoT(todo, value, ty)
  end Todo

  def prop(p: Prop[AST]): AST[Prop[AST]] = AST.PropT(p)
  
  // These are treated as user-defined, so there needs to be code generated for them.
  def Utility_Defs: BindTs = List(
    N_0, N_1,
    Z_0, Z_1,
    nat_max
  )
end ASTCore

