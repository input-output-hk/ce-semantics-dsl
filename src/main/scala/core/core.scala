package core

import trees.*
import trees.ast.*
import trees.cgtree.NameType

import scala.annotation.{tailrec, targetName}
import scala.deriving.Mirror

// Core language.
// Using the tagless-final (or finally tagless) approach, originally in OCaml and Haskell:
//   - https://okmij.org/ftp/tagless-final/course/lecture.pdf
//   - https://okmij.org/ftp/tagless-final/JFP.pdf
trait Core[R[_]]:
  // A more specific Representation, used for bindings
  type Bind[T] <: R[T]

  object TypeInference extends TypeInference[R]
  import TypeInference.given

  def str(s: String): R[String]
  def str_len(s: R[String]): R[Nat]
  // TODO Return an Option or a Result in order to take into account
  // an index that is out of bounds.
  // Note we do not have exceptions in Core[I]
  // We assume a 0-based index.
  def str_char_at(s: R[String], i: R[Nat]): R[Char]
  def str_concat(a: R[String], b: R[String]): R[String]
  extension (s: R[String])
    @targetName("__ext_str_char_at")
    def apply(i: R[Nat]): R[Char] = str_char_at(s, i)
    @targetName("__ext_str_length")
    def length: R[Nat] = str_len(s)
    @targetName("__ext_str_plusplus")
    def ++(t: R[String]): R[String] = str_concat(s, t)
  end extension

  def char(c: Char): R[Char]
  def char_to_str(c: R[Char]): R[String]
  def char_eq(a: R[Char], b: R[Char]): R[Bool]
  extension (a: R[Char])
    @targetName("__ext_char_to_str")
    def to_str: R[String] = char_to_str(a)
    @targetName("__ext_char_eqeqeq")
    def ===(b: R[Char]): R[Bool] = char_eq(a, b)
  end extension

  def name(s: scala.Predef.String): Name
  @targetName("__name_valueOf")
  def name[S <: Name : ValueOf]: Name = valueOf[S]

  // syntactic convenience for function parameter names
  inline def p[S <: Name : ValueOf]: Name = name[S]

  // syntactic convenience for function names
  inline def f[S <: Name : ValueOf]: Name = name[S]

  // syntactic convenience for variable names
  inline def v[S <: Name : ValueOf]: Name = name[S]

  extension(a: Name)
    @targetName("__ext_ident_def_val")
    def :=[T: Ty](v: R[T]): Bind[T] = bind(a, v)
    @targetName("__ext_ident_def_fun1")
    def -->[A: Ty, B: Ty](f: Fun1[R, A, B]): R[Fun1[R, A, B]] = fun1(a, f)
  end extension
  extension(ab: (Name, Name))
    @targetName("__ext_ident_def_fun2")
    inline def -->[A: Ty, B: Ty, Z: Ty](f: Fun2[R, A, B, Z]): R[Fun2[R, A, B, Z]] =
      fun2(ab._1, ab._2, f)
  end extension
  extension(abc: (Name, Name, Name))
    @targetName("__ext_ident_def_fun3")
    inline def -->[A: Ty, B: Ty, C: Ty, Z: Ty](f: Fun3[R, A, B, C, Z]): R[Fun3[R, A, B, C, Z]] =
      fun3(abc._1, abc._2, abc._3, f)
  end extension
  extension(abcd: (Name, Name, Name, Name))
    @targetName("__ext_ident_def_fun4")
    inline def -->[A: Ty, B: Ty, C: Ty, D: Ty, Z: Ty](f: Fun4[R, A, B, C, D, Z]): R[Fun4[R, A, B, C, D, Z]] =
      fun4(abcd._1, abcd._2, abcd._3, abcd._4, f)
  end extension
  extension(abcde: (Name, Name, Name, Name, Name))
    @targetName("__ext_ident_def_fun5")
    inline def -->[A: Ty, B: Ty, C: Ty, D: Ty, E: Ty, Z: Ty](f: Fun5[R, A, B, C, D, E, Z]): R[Fun5[R, A, B, C, D, E, Z]] =
      fun5(abcde._1, abcde._2, abcde._3, abcde._4,  abcde._5, f)
  end extension

  // TODO missing constructor for scala.Option[A]?
  def option_none[T: Ty](): R[Option[R[T]]]
  def option_some[T: Ty](value: R[T]): R[Option[R[T]]]

  def bool(b: Bool): R[Bool]
  def bool_or (a: R[Bool], b: R[Bool]): R[Bool]
  def bool_and(a: R[Bool], b: R[Bool]): R[Bool]
  def bool_not(a: R[Bool]): R[Bool]
  extension(a: R[Bool])
    @targetName("__ext_bool_or)")
    inline def ||(b: R[Bool]): R[Bool] = bool_or(a, b)
    @targetName("__ext_bool_and)")
    inline def &&(b: R[Bool]): R[Bool] = bool_and(a, b)
    @targetName("__ext_boot_not")
    inline def unary_! : R[Bool] = bool_not(a)
  end extension

  // ℕ = Natural numbers
  def nat(x: scala.Int): R[Nat]
  def nat_eq (a: R[Nat], b: R[Nat]): R[Bool]
  def nat_lt (a: R[Nat], b: R[Nat]): R[Bool]
  def nat_gteq(a: R[Nat], b: R[Nat]): R[Bool]
  def nat_add(a: R[Nat], b: R[Nat]): R[Nat]
  def nat_sub(a: R[Nat], b: R[Nat]): R[Nat]
  def nat_mul(a: R[Nat], b: R[Nat]): R[Nat]
  def nat_div(a: R[Nat], b: R[Nat]): R[Nat]
  extension(n: R[Nat])
    @targetName("__ext_nat_eq")
    inline def ===(n2: R[Nat]): R[Bool] = nat_eq(n, n2)
    @targetName("__ext_nat_lt")
    inline def <(n2: R[Nat]): R[Bool] = nat_lt(n, n2)
    @targetName("__ext_nat_gteq")
    inline def >=(n2: R[Nat]): R[Bool] = nat_gteq(n, n2)
    @targetName("__ext_nat_plus")
    inline def +(n2: R[Nat]): R[Nat] = nat_add(n, n2)
    @targetName("__ext_n_minus")
    inline def -(n2: R[Nat]): R[Nat] = nat_sub(n, n2)
    @targetName("__ext_nat_star")
    inline def *(n2: R[Nat]): R[Nat] = nat_mul(n, n2)
    @targetName("__ext_nat_div")
    inline def /(n2: R[Nat]): R[Nat] = nat_div(n, n2)
  end extension

  // ℤ = Integers or else known as "Zatural" numbers (to rhyme with "Natural").
  // Note (loverdos) No, they are not known as "Zatural", I just came up with it. 
  def zat(x: scala.Int): R[Zat]
  def zat_of_nat(nat: R[Nat]): R[Zat]
  def zat_mul(a: R[Zat], b: R[Zat]): R[Zat]
  def zat_add(a: R[Zat], b: R[Zat]): R[Zat]
  extension(a: R[Zat])
    @targetName("__ext_zat_star")
    inline def *(b: R[Zat]): R[Zat] = zat_mul(a, b)
    @targetName("__ext_zat_plus")
    inline def +(b: R[Zat]): R[Zat] = zat_add(a, b)
  end extension

  // Pairs
  def pair_new   [A: Ty, B: Ty](a: R[A], b: R[B]): R[Pair[R[A], R[B]]]
  def pair_first [A: Ty, B: Ty](pair: R[Pair[R[A], R[B]]]): R[A]
  def pair_second[A: Ty, B: Ty](pair: R[Pair[R[A], R[B]]]): R[B]

  extension [A: Ty, B: Ty](pair: R[Pair[R[A], R[B]]])
    inline def first(): R[A] = pair_first(pair)
    inline def second(): R[B] = pair_second(pair)
  end extension


  // Maps (we assume functional/persistent maps)
  //
  // Note in general, the computational aspects of the constructs provided or assumed by Core and its
  //      interpretations should be made clear.
  //
  // Map constructor
  // Here we may add a more traditional specification, like an new map is always an empty map.
  // So: `foreach K, V: Type`, the condition `map_is_empty(map_new[K, V]())` is always true
  def map_new[K: Ty, V: Ty](): R[Map[R[K], R[V]]]
  def map_get[K: Ty, V: Ty](map: R[Map[R[K], R[V]]], key: R[K]): R[V] // TODO Do we need Option?
  def map_values[K: Ty, V: Ty](map: R[Map[R[K], R[V]]]): R[Seq[R[V]]]
  def map_put[K: Ty, V: Ty](map: R[Map[R[K], R[V]]], key: R[K], value: R[V]): R[Map[R[K], R[V]]]

  extension [K: Ty, V: Ty](map: R[Map[R[K], R[V]]])
    inline def get(key: R[K]): R[V] = map_get(map, key)
    inline def values(): R[Seq[R[V]]] = map_values(map)
    inline def put(key: R[K], value: R[V]): R[Map[R[K], R[V]]] = map_put(map, key, value)
  end extension

  // Sets (we assume functional/persistent sets)
  // set constructor
  def set_new[K: Ty](): R[Set[R[K]]]
  def set_add[K: Ty](set: R[Set[R[K]]], elem: R[K]): R[Set[R[K]]]
  def set_of_elem[K: Ty](elem: R[K]): R[Set[R[K]]] = set_add(set_new[K](), elem)

  // Sequences (we assume functional/persistent sequences)
  def seq_new[K: Ty](elems: R[K]*): R[Seq[R[K]]]
  def seq_add[K: Ty](seq: R[Seq[R[K]]], elem: R[K]): R[Seq[R[K]]]
  def seq_map[K: Ty, L: Ty](seq: R[Seq[R[K]]], f: R[R[K] => R[L]]): R[Seq[R[L]]] // TODO can be derived if we have foreach
  def seq_fold_left[K: Ty, L: Ty](seq: R[Seq[R[K]]], initial: R[L], f: R[Fun2[R, L, K, L]]): R[L]
  // given a fold, this is the denotational semantics of computing the sum
  // TODO This should be a helper function somewhere else (so a let binding)
  // Note If we keep it here then the AST interpretation will treat `seq_sum` as an
  //      inline function (a macro), so the implementation will always be substituted
  //      (instead of creating a function call)
  inline def seq_sum[K: Ty](seq: R[Seq[R[K]]], k_zero: R[K], seq_add: R[Fun2[R, K, K, K]]): R[K] =
    seq_fold_left[K, K](seq, k_zero, seq_add)
  end seq_sum

  extension [K: Ty](seq: R[Seq[R[K]]])
    @targetName("__ext_seq_add")
    inline def add(elem: R[K]): R[Seq[R[K]]] = seq_add(seq, elem)
    @targetName("__ext_seq_map")
    inline def map[L: Ty](f: R[R[K] => R[L]]): R[Seq[R[L]]] = seq_map(seq, f)
    @targetName("__ext_seq_fold_left")
    inline def fold_left[L: Ty](initial: R[L], f: R[Fun2[R, L, K, L]]): R[L] = seq_fold_left(seq, initial, f)
    @targetName("__ext_seq_sum")
    inline def sum(k_zero: R[K], seq_add: R[Fun2[R, K, K, K]]): R[K] = seq_sum(seq, k_zero, seq_add)
  end extension

  // to reduce some verbosity when 0, 1 are needed
  implicit def implicit_N_0(z: 0): R[Nat] = N_0
  implicit def implicit_N_1(z: 1): R[Nat] = N_1
  implicit def implicit_Z_0(z: 0): R[Zat] = Z_0
  implicit def implicit_Z_1(z: 1): R[Zat] = Z_1
  
  def fun0[Z: Ty](f: Fun0[R, Z]): R[Fun0[R, Z]]
  def app0[Z: Ty](f: R[Fun0[R, Z]]): R[Z]
  extension[Z: Ty](f: R[Fun0[R, Z]])
    inline def apply(): R[Z] = app0(f)
  end extension

  def fun1[A: Ty, Z: Ty](a: Name, f: Fun1[R, A, Z]): R[Fun1[R, A, Z]]
  def app1[A: Ty, Z: Ty](f: R[Fun1[R, A, Z]], a: R[A]): R[Z]
  extension[A: Ty, Z: Ty](f: R[Fun1[R, A, Z]])
    inline def apply(a: R[A]): R[Z] = app1(f, a)
  end extension

  def fun2[A: Ty, B: Ty, Z: Ty](a: Name, b: Name, f: Fun2[R, A, B, Z]): R[Fun2[R, A, B, Z]]
  def app2[A: Ty, B: Ty, Z: Ty](f: R[Fun2[R, A, B, Z]], a: R[A], b: R[B]): R[Z]
  extension[A: Ty, B: Ty, Z: Ty](f: R[Fun2[R, A, B, Z]])
    inline def apply(a: R[A], b: R[B]): R[Z] = app2(f, a, b)
  end extension

  def fun3[A: Ty, B: Ty, C: Ty, Z: Ty](a: Name, b: Name, c: Name, f: Fun3[R, A, B, C, Z]): R[Fun3[R, A, B, C, Z]]
  def app3[A: Ty, B: Ty, C: Ty, Z: Ty](f: R[Fun3[R, A, B, C, Z]], a: R[A], b: R[B], c: R[C]): R[Z]
  extension[A: Ty, B: Ty, C: Ty, Z: Ty](f: R[Fun3[R, A, B, C, Z]])
    inline def apply(a: R[A], b: R[B], c: R[C]): R[Z] = app3(f, a, b, c)
  end extension

  def fun4[A: Ty, B: Ty, C: Ty, D: Ty, Z: Ty](a: Name, b: Name, c: Name, d: Name, f: Fun4[R, A, B, C, D, Z]): R[Fun4[R, A, B, C, D, Z]]
  def app4[A: Ty, B: Ty, C: Ty, D: Ty, Z: Ty](f: R[Fun4[R, A, B, C, D, Z]], a: R[A], b: R[B], c: R[C], d: R[D]): R[Z]
  extension[A: Ty, B: Ty, C: Ty, D: Ty, Z: Ty](f: R[Fun4[R, A, B, C, D, Z]])
    inline def apply(a: R[A], b: R[B], c: R[C], d: R[D]): R[Z] = app4(f, a, b, c, d)
  end extension

  def fun5[A: Ty, B: Ty, C: Ty, D: Ty, E: Ty, Z: Ty](a: Name, b: Name, c: Name, d: Name, e: Name, f: Fun5[R, A, B, C, D, E, Z]): R[Fun5[R, A, B, C, D, E, Z]]
  def app5[A: Ty, B: Ty, C: Ty, D: Ty, E: Ty, Z: Ty](f: R[Fun5[R, A, B, C, D, E, Z]], a: R[A], b: R[B], c: R[C], d: R[D], e: R[E]): R[Z]
  extension[A: Ty, B: Ty, C: Ty, D: Ty, E: Ty, Z: Ty](f: R[Fun5[R, A, B, C, D, E, Z]])
    inline def apply(a: R[A], b: R[B], c: R[C], d: R[D], e: R[E]): R[Z] = app5(f, a, b, c, d, e)
  end extension

  // let binding
  def bind[T: Ty](name: Name, v: R[T]): Bind[T]

  // Add a pre-condition to a function binding.
  // Note how we require evidence that the type of the function and the type of the pre-condition should match.
  def bind_add_precond3[T: Ty, A, B, C, Z](
    bind: Bind[T],
    cond: R[Fun3[R, A, B, C, Prop[R]]]
  )(implicit ev: T =:= Fun3[R, A, B, C, Z]): Bind[T]
  // `Result` here is the function result type, so for a function of 3 parameters the post-condition needs to be of
  // 4 parameters, including the result.
  def bind_add_postcond3[T: Ty, A, B, C, Z, Result](
    bind: Bind[T],
    cond: R[Fun4[R, A, B, C, Result, Prop[R]]]
  )(implicit ev: T =:= Fun3[R, A, B, C, Z]): Bind[T]

  // TODO Use the actual type of `T` here and get rid of the evidence parameter below.
  extension [T: Ty](bind: Bind[T])
    def add_precond3 [A, B, C, Z](
      cond: R[Fun3[R, A, B, C, Prop[R]]]
    )(implicit ev: T =:= Fun3[R, A, B, C, Z]): Bind[T] =

      bind_add_precond3(bind, cond)
    end add_precond3

    def add_postcond3[A, B, C, Res, Z](
      cond: R[Fun4[R, A, B, C, Res, Prop[R]]]
    )(implicit ev: T =:= Fun3[R, A, B, C, Z]): Bind[T] =

      bind_add_postcond3(bind, cond)
    end add_postcond3

  end extension

  def _let[A: Ty, Z: Ty](bind: Bind[A], in: Fun1[R, A, Z]): R[Z]
  inline def let[A: Ty, Z: Ty](bind: Bind[A])(in: Fun1[R, A, Z]): R[Z] = _let(bind, in)

  // If Then Else
  def ifte[T: Ty](i: R[Bool], t: => R[T], e: => R[T]): R[T]
  def if_then_else[T: Ty](i: R[Bool])(t: => R[T])(e: => R[T]): R[T] = ifte(i, t, e)

  ///////////////////////////////////////////////////////////////////////////////
  // Structs
  ///////////////////////////////////////
  // A field that is not attached to a struct.
  // The idea is that it can be reused for the definition of more than one Structs
  def bare_field[N <: FieldName: ValueOf, F: Ty]: BareField[N, F] =
    BareField(valueOf[N], summon[Ty[F]])
  end bare_field

  transparent inline def struct_constructorX[Struct <: AnyStruct, Args <: Tuple](
    using StructM: Mirror.ProductOf[Struct],
    ev: StructM.MirroredElemTypes =:= Args
  ): Args => Struct =
    StructM.fromProduct(_: Args)
  end struct_constructorX

  // Convenience over `struct_constructorX`, since `Args` can be computed, but if you try to infer a variable
  // type in IntelliJ it will not work as expected:
  // `struct_constructorX` though infers the precise type in the IDE.
  transparent inline def struct_constructor[Struct <: AnyStruct](using StructM: Mirror.ProductOf[Struct]) =
    StructM.fromProduct(_: StructM.MirroredElemTypes)
  end struct_constructor

  transparent inline def def_struct[Struct <: AnyStruct : Ty, Fields <: Tuple, Args <: Tuple](
    name: Name,
    fields: Fields,
    constr: Args => Struct
  )/*(using ev: Args =:= ...)*/ : StructDef[Struct, Fields, Args] =
    // TODO Check statically that the types `Fields` and `Args` are consistent.
    //      In fact `Args` should be derivable from `Fields`.
    // TODO Derive the name from StructTy
    val struct_ty = summon[Ty[Struct]]
    require(struct_ty.repr == name) // to maintain some consistency in code generation
    StructDef(name, struct_ty, fields, constr)
  end def_struct

  transparent inline def def_struct[Struct <: AnyStruct : Ty, Fields <: Tuple, Args <: Tuple](
    fields: Fields,
    constr: Args => Struct
  )/*(using ev: Args =:= ...)*/ : StructDef[Struct, Fields, Args] =
    // TODO Check statically that the types `Fields` and `Args` are consistent.
    //      In fact `Args` should be derivable from `Fields`.
    val struct_ty = summon[Ty[Struct]]
    val name = struct_ty.repr
    def_struct(name, fields, constr)
  end def_struct

  transparent inline def def_field[Struct <: AnyStruct, Fields <: Tuple, Args <: Tuple, N <: FieldName, F](
    struct: StructDef[Struct, Fields, Args],
    field: BareField[N, F],
    getter: Struct => R[F]
  ): FieldDef[Struct, N, F, R] =
    FieldDef(struct, field, getter)
  end def_field

  def struct_new[Struct <: AnyStruct: Ty, Fields <: Tuple, Args <: Tuple](
    struct_def: StructDef[Struct, Fields, Args],
    args: Args
  ): R[Struct]

  extension [Struct <: AnyStruct: Ty, Fields <: Tuple, Args <: Tuple](struct_def: StructDef[Struct, Fields, Args])
  // Note Try to `inline` this and say goodbye to the compiler (3.1.x).
    def apply(args: Args): R[Struct] = struct_new(struct_def, args)
  end extension

  // Dot operator: struct dot field
  //           or: struct.field
  //               if `.` were overridable
  def dot[Struct <: AnyStruct: Ty, N <: FieldName, F: Ty](
    struct: R[Struct],
    field_def: FieldDef[Struct, N, F, R]
  ): R[F]

  extension [Struct <: AnyStruct: Ty](struct: R[Struct])
    @targetName("__ext_struct_apply")
    def apply[N <: FieldName, F: Ty](field_def: FieldDef[Struct, N, F, R]): R[F] = dot(struct, field_def)
    @targetName("__ext_struct_get")
    def get  [N <: FieldName, F: Ty](field_def: FieldDef[Struct, N, F, R]): R[F] = dot(struct, field_def)
  end extension

  extension [Struct <: AnyStruct: Ty, N <: FieldName, F: Ty](field_def: FieldDef[Struct, N, F, R])
     // so, basically: field_def(struct) = struct(field_def)
     // because why not. Let the best syntax win!
    def apply(struct: R[Struct]): R[F] = struct(field_def)
  end extension

  def Todo[T: Ty](todo: String, value: R[T]): R[T]

  //object Props:
  // This is meant to be used internally, as a building block block for the eDSL itself, not programs using the eDSL.
  // TODO enforce the nature of its use via Scala facilities.
  // See also AST transformation and code generation for properties.
  def prop(p: Prop[R]): R[Prop[R]]

  // This is meant to be used internally, as a building block block for the eDSL itself, not programs using the eDSL.
  // TODO enforce the nature of its use via Scala facilities.
  // See also AST transformation and code generation for properties.
  def prop_bool(expr: R[Bool]): R[Prop[R]] =
    prop( Prop.BoolP(expr) )
  end prop_bool

  // This is meant to be used internally, as a building block block for the eDSL itself, not programs using the eDSL.
  // TODO enforce the nature of its use via Scala facilities.
  // See also AST transformation and code generation for properties.
  def prop_forall_in[X <: (Nat | Zat) : Ty](
    name: Name,
    bounds: (R[X], R[X]),
    upper_bound_is: BoundIs,
    predicate: Fun1[R, X, Bool]
  ): R[Prop[R]] =
    val predicate_f = fun1(name, predicate)
    val ty = summon[Ty[X]]

    prop( Prop.ForAllInRangeP(name, ty, bounds, upper_bound_is, predicate_f) )
  end prop_forall_in

  def prop_forall_in[N <: Name : ValueOf, X <: (Nat | Zat) : Ty](
    bounds: (R[X], R[X]),
    upper_bound_is: BoundIs,
    predicate: Fun1[R, X, Bool]
  ): R[Prop[R]] =
    val name = valueOf[N]
    prop_forall_in(name, bounds, upper_bound_is, predicate)
  end prop_forall_in
  //end Props

  /////////////////////////////////////////////////////////
  // Utilities
  //
  // These are meant to be defined solely by previous definitions in Core.
  //
  // Note We could use a separate trait and mix it in on demand.
  //
  // Note A code generator should generate code for these, as they are not primitives.
  /////////////////////////////////////////////////////////

  lazy val N_0 = v["N_0"] := nat(0)
  lazy val N_1 = v["N_1"] := nat(1)
  lazy val Z_0 = v["Z_0"] := zat_of_nat(N_0)
  lazy val Z_1 = v["Z_1"] := zat_of_nat(N_1)

  def impl_nat_max(a: R[Nat], b: R[Nat]): R[Nat] =
    if_then_else(a >= b)(a)(b)
  end impl_nat_max

  lazy val nat_max = f["nat_max"] := (p["a"], p["b"]) --> impl_nat_max
  inline def max(a: R[Nat], b: R[Nat]): R[Nat] = nat_max(a, b)

  extension (ab: (R[Nat], R[Nat]))
    @targetName("__ext_nat_max")
    inline def max: R[Nat] = nat_max(ab._1, ab._2)
  end extension
end Core
