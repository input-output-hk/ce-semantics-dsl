package trees
package ast

import core.*
import org.apache.commons.text.StringEscapeUtils

import scala.annotation.constructorOnly


// The front-end Abstract Syntax Tree, which corresponds to the eDSL constructs.
// See also the CGTree, which is for code generation. 
enum AST[T](val ty: Ty[T]):
  case BindT[T](
    name: String,
    value: AST[T],
    override val ty: Ty[T],
    creation: BindTCreation,
    pre_cond: List[AST[?]] = Nil, // List[AST[Function that returns AST[Prop]]]
    post_cond: List[AST[?]] = Nil // List[AST[Function that returns AST[Prop]]]
  ) extends AST(ty)
  
  case LetT[A, Z](
    name: String,
    value: AST[A],
    in: AST[A] => AST[Z],
    body: AST[Z], // no let rec
    a_ty: Ty[A],
    override val ty: Ty[Z],
  ) extends AST[Z](ty)

  // We know what [[PropIs]] this is from context. In code generated trees we state this explicitly though.
  case PropT(prop: Prop[AST]) extends AST(Ty.PropTy[AST]())

  case IfThenElseT[T](
    _if: AST[Bool],
    _then: AST[T],
    _else: AST[T],
    override val ty: Ty[T]
  ) extends AST(ty)

  case ForLoopT[T /*<: Nat | Zat*/](
    var_name: String,
    var_ty: Ty[T],
    lower: AST[T],
    upper: AST[T],
    upper_bound_is: BoundIs,
    body: AST[?]
  ) extends AST(Ty.UnitTy)
  
  case ParamT[T](name: String, override val ty: Ty[T]) extends AST(ty)

  case Fun0T[Z](
    f_applied: () => AST[Z], // f()
    f: Fun0[AST, Z],
    override val ty: Ty[Fun0[AST, Z]]
  ) extends AST(ty)

  case App0T[Z](
    f: AST[() => AST[Z]],
    override val ty: Ty[Z]
  ) extends AST(ty)

  case Fun1T[A, Z](
    a: ParamT[A],
    f_applied: () => AST[Z], // f(a)
    f: Fun1[AST, A, Z],
    override val ty: Ty[Fun1[AST, A, Z]]
  ) extends AST(ty)

  case App1T[A, Z](
    f: AST[Fun1[AST, A, Z]],
    a: AST[A],
    override val ty: Ty[Z]
  ) extends AST(ty)

  case Fun2T[A, B, Z](
    a: ParamT[A], b: ParamT[B],
    f_applied: () => AST[Z], // f(a, b)
    f: Fun2[AST, A, B, Z],
    override val ty: Ty[Fun2[AST, A, B, Z]]
  ) extends AST(ty)

  case App2T[A, B, Z](
    f: AST[Fun2[AST, A, B, Z]],
    a: AST[A], b: AST[B],
    override val ty: Ty[Z]
  ) extends AST(ty)

  case Fun3T[A, B, C, Z](
    a: ParamT[A], b: ParamT[B], c: ParamT[C],
    f_applied: () => AST[Z], // f(a, b, c)
    f: Fun3[AST, A, B, C, Z],
    override val ty: Ty[Fun3[AST, A, B, C, Z]]
  ) extends AST(ty)

  case App3T[A, B, C, Z](
    f: AST[Fun3[AST, A, B, C, Z]],
    a: AST[A], b: AST[B], c: AST[C],
    override val ty: Ty[Z]
  ) extends AST(ty)

  case Fun4T[A, B, C, D, Z](
    a: ParamT[A], b: ParamT[B], c: ParamT[C], d: ParamT[D],
    f_applied: () => AST[Z], // f(a, b, c, d)
    f: Fun4[AST, A, B, C, D, Z],
    override val ty: Ty[Fun4[AST, A, B, C, D, Z]]
  ) extends AST(ty)

  case App4T[A, B, C, D, Z](
    f: AST[Fun4[AST, A, B, C, D, Z]],
    a: AST[A], b: AST[B], c: AST[C], d: AST[D],
    override val ty: Ty[Z]
  ) extends AST(ty)

  case Fun5T[A, B, C, D, E, Z](
    a: ParamT[A], b: ParamT[B], c: ParamT[C], d: ParamT[D], e: ParamT[E],
    f_applied: () => AST[Z], // f(a, b, c, d, e)
    f: Fun5[AST, A, B, C, D, E, Z],
    override val ty: Ty[Fun5[AST, A, B, C, D, E, Z]]
  ) extends AST(ty)

  case App5T[A, B, C, D, E, Z](
    f: AST[Fun5[AST, A, B, C, D, E, Z]],
    a: AST[A], b: AST[B], c: AST[C], d: AST[D], e: AST[E],
    override val ty: Ty[Z]
  ) extends AST(ty)

  // Unit
  //case UnitT extends AST(Ty.UnitTy)
  
  // String
  case StringT      (str: String)      extends AST(Ty.StringTy)
  case StringLenT   (str: AST[String]) extends AST(Ty.NatTy)
  case StringCharAtT(str: AST[String], index: AST[Nat]) extends AST(Ty.CharTy)
  case StringConcatT(a: AST[String], b: AST[String]) extends AST(Ty.StringTy)

  // Char
  case CharT(c: Char) extends AST(Ty.CharTy)
  case CharToStrT(c: AST[Char]) extends AST(Ty.StringTy)
  case CharEqT(a: AST[Char], b: AST[Char]) extends AST(Ty.BoolTy)

  // Option
  //case OptionT[T](v: Option[AST[T]]) extends AST[Option[AST[T]]]
  case NoneT[T](              override val ty: Ty[Option[AST[T]]]) extends AST(ty)
  case SomeT[T](some: AST[T], override val ty: Ty[Option[AST[T]]]) extends AST(ty)

  // Bool
  case BoolT              (bool: Bool)                                      extends AST(Ty.BoolTy)
  case BoolBinOpT         (op: BoolBinOp,       a: AST[Bool], b: AST[Bool]) extends AST(Ty.BoolTy)
  case ComparisonBinOpT[T](op: ComparisonBinOp, a: AST[T],    b: AST[T])    extends AST(Ty.BoolTy)
  case BoolNotT           (a: AST[Bool])                                    extends AST(Ty.BoolTy)

  // Nat, Zat & operators
  case NatT     (nat: Nat)      extends AST(Ty.NatTy)
  case ZatT     (zat: Zat)      extends AST(Ty.ZatTy)
  case ZatOfNatT(nat: AST[Nat]) extends AST(Ty.ZatTy)
  case ArithBinOpT[T](op: ArithBinOp, a: AST[T], b: AST[T]) extends AST(a.ty)

  // Pair
  case PairNewT   [A, B](a: AST[A], b: AST[B], override val ty: Ty[Pair[AST[A], AST[B]]]) extends AST(ty)
  case PairFirstT [A, B](pair: AST[Pair[AST[A], AST[B]]], a_ty: Ty[A], b_ty: Ty[B])       extends AST(a_ty)
  case PairSecondT[A, B](pair: AST[Pair[AST[A], AST[B]]], a_ty: Ty[A], b_ty: Ty[B])       extends AST(b_ty)

  // Map
  case MapNewT[K, V](k_ty: Ty[K], v_ty: Ty[V], override val ty: Ty[Map[AST[K], AST[V]]]) extends AST(ty)
  case MapGetT[K, V](map: AST[Map[AST[K], AST[V]]], key: AST[K], override val ty: Ty[V]) extends AST(ty) // TODO Option
  case MapPutT[K, V](map: AST[Map[AST[K], AST[V]]], key: AST[K], value: AST[V]) extends AST(map.ty)
  case MapValuesT[K, V](map: AST[Map[AST[K], AST[V]]], k_ty: Ty[K], v_ty: Ty[V], override val ty: Ty[Seq[AST[V]]]) extends AST(ty)

  // Set
  case SetNewT[K](item_ty: Ty[K],                      override val ty: Ty[Set[AST[K]]]) extends AST(ty)
  case SetAddT[K](set: AST[Set[AST[K]]], item: AST[K], override val ty: Ty[Set[AST[K]]]) extends AST(ty)

  // Seq
  case SeqNewT[K](items: Seq[AST[K]], item_ty: Ty[K], override val ty: Ty[Seq[AST[K]]]) extends AST(ty)
  case SeqAddT[K](seq: AST[Seq[AST[K]]], item: AST[K], item_ty: Ty[K]) extends AST(seq.ty)
  case SeqMapT[K, L](seq: AST[Seq[AST[K]]], f: AST[Fun1[AST, K, L]], k_ty: Ty[K], l_ty: Ty[L], override val ty: Ty[Seq[AST[L]]]) extends AST(ty)
  case SeqFoldLeftT[K, L](seq: AST[Seq[AST[K]]], initial: AST[L], f: AST[Fun2[AST, L, K, L]], k_ty: Ty[K], l_ty: Ty[L]) extends AST(l_ty)
  
  case StructNewT[
    Struct <: AnyStruct,
    Fields <: Tuple,
    Params <: Tuple
  ](
    struct_def: StructDef[Struct, Fields, Params],
    params: Params,
    override val ty: Ty[Struct]
  ) extends AST(ty)

  case StructDotT[
    Struct <: AnyStruct,
    N <: FieldName,
    F
  ](
    struct: AST[Struct],
    field_def: FieldDef[Struct, N, F, AST],
    override val ty: Ty[F]
  ) extends AST(ty)

  // Misc
  case TodoT[T](todo: String, ast: AST[T], override val ty: Ty[T]) extends AST(ty)

  // for debugging
  private def to_short_str_internal: String =
    val internal =
      this match
        case AST.BindT(name, value, _, _, _, _) => s"${name}"
        case AST.ParamT(name, _) => name
        case AST.App1T(fun, x, _) => s"APP(${fun.to_short_str} ${x.to_short_str})"
        case AST.ArithBinOpT(op, a, b) => s"OP(${op.op_str} ${a.to_short_str} ${b.to_short_str})"
        case AST.PropT(prop) => s"$prop"
        
        case _ => ""
      end match

    val external =
      this match
        case _: AST.BindT[?] => ""
        case _: AST.ArithBinOpT[?] => ""
        case _: AST.App1T[?, ?] => ""
        case _ => getClass.getSimpleName
      end match

    if external.isEmpty then s"$internal" else s"$external($internal)"
  end to_short_str_internal

  // for debugging
  def to_short_str: String =
    to_short_str_internal
  end to_short_str

  // for debugging
  // TODO either delete or implement fully
  def to_typed_str: String =
    def escaped_quoted(what: String, quote: Char): String =
      val escaped = StringEscapeUtils.escapeJava(what)
      s"$quote$escaped$quote"
    end escaped_quoted
    
    val inner =
      this match
        case AST.NatT(nat) => nat.v.toString()
        case AST.ZatT(zat) => zat.v.toString()
        case AST.BoolT(bool) => bool.toString()
        case AST.CharT(c) => escaped_quoted(c.toString, '\'')
        case AST.CharToStrT(c) => c.to_typed_str
        case AST.StringT(str) => escaped_quoted(str, '"')
        case AST.StringLenT(str) => str.to_typed_str
        case AST.StringConcatT(a, b) => s"${a.to_typed_str}, ${b.to_typed_str}"
        case AST.BindT(ident, value, _, _, _, _) => s"${ident}: ${value}"
        case AST.ParamT(ident, ty) => s"${ident}: ${ty.untag.repr}"
        case AST.App1T(AST.BindT(name, _, _, _, _, _), a, _) => s"${name}(${a.to_typed_str})"
        case AST.App1T(fun, a, _) => s"${fun.to_typed_str}(${a.to_typed_str})"
        case AST.App3T(AST.BindT(name, _, _, _, _, _), a, b, c, _) => s"${name}(${a.to_typed_str}, ${b.to_typed_str}, ${c.to_typed_str})"
        case AST.App3T(fun, a, b, c, _) => s"${fun.to_typed_str}(${a.to_typed_str}, ${b.to_typed_str}, ${c.to_typed_str})"
        case AST.IfThenElseT(_if, _then, _else, _) => s"_[IF]_ ${_if.to_typed_str} _[THEN]_ ${_then.to_typed_str} _[ELSE]_ ${_else.to_typed_str}"
        case AST.ComparisonBinOpT(op, a, b) => s"${op.op_str} ${a.to_typed_str} ${b.to_typed_str}"
        case AST.ArithBinOpT(op, a, b) => s"${op.op_str} ${a.to_typed_str} ${b.to_typed_str}"
        case AST.BoolBinOpT(op, a, b)  => s"${op.op_str} ${a.to_typed_str} ${b.to_typed_str}"
        case AST.PropT(prop) => s"$prop"

        // TODO Add other AST nodes ...
        case _ => s"_: ${ty.untag.repr}"
      end match
    val outer = getClass.getSimpleName
    outer + "(" + inner + ")"
  end to_typed_str


  def to_original_str: String = super.toString

  override def toString: String = to_typed_str
end AST

enum BindTCreation:
  case BindTeDSL     // original scope using the bind() API
  case BindTRefExprT // used to create a CGTRee.RefExprT. This is, in a sense, a "synthetic" BindT
  case BindTProp     // used to create a Property, such as a pre-/post- condition. This is, in a sense, a "synthetic" BindT
end BindTCreation

type Fun0T_? = AST.Fun0T[?]
type Fun1T_? = AST.Fun1T[?, ?]
type Fun2T_? = AST.Fun2T[?, ?, ?]
type Fun3T_? = AST.Fun3T[?, ?, ?, ?]
type Fun4T_? = AST.Fun4T[?, ?, ?, ?, ?]
type Fun5T_? = AST.Fun5T[?, ?, ?, ?, ?, ?]

type FunT = Fun0T_? | Fun1T_? | Fun2T_? | Fun3T_? | Fun4T_? | Fun5T_?

enum FunTKind:
  case RegularFunT
  // function used for property checking
  case PropFunT(prop_is: PropIs)
end FunTKind

type App0T_? = AST.App0T[?]
type App1T_? = AST.App1T[?, ?]
type App2T_? = AST.App2T[?, ?, ?]
type App3T_? = AST.App3T[?, ?, ?, ?]
type App4T_? = AST.App4T[?, ?, ?, ?, ?]
type App5T_? = AST.App5T[?, ?, ?, ?, ?, ?]

type ASTs    = List[AST[?]]
type BindTs  = List[AST.BindT[?]]
type ParamTs = List[AST.ParamT[?]]


// data holder for FunT pattern matching
case class FunTData(fun_t: FunT, params: ParamTs, f_applied: () => AST[?], ty: Ty[?])

extension (ast: FunT)
  def as_FunTData: FunTData =
    ast match
      case fun_t @ AST.Fun0T(               f_applied, _, ty) => FunTData(fun_t, List(),              f_applied, ty)
      case fun_t @ AST.Fun1T(a,             f_applied, _, ty) => FunTData(fun_t, List(a),             f_applied, ty)
      case fun_t @ AST.Fun2T(a, b,          f_applied, _, ty) => FunTData(fun_t, List(a, b),          f_applied, ty)
      case fun_t @ AST.Fun3T(a, b, c,       f_applied, _, ty) => FunTData(fun_t, List(a, b, c),       f_applied, ty)
      case fun_t @ AST.Fun4T(a, b, c, d,    f_applied, _, ty) => FunTData(fun_t, List(a, b, c, d),    f_applied, ty)
      case fun_t @ AST.Fun5T(a, b, c, d, e, f_applied, _, ty) => FunTData(fun_t, List(a, b, c, d, e), f_applied, ty)
  end as_FunTData
end extension

object FunT:
  def unapply[Z](ast: AST[Z]): Option[FunTData] =
    ast match
      case fun_t: FunT => Some(fun_t.as_FunTData)
      case _ => None
  end unapply
end FunT

//object FunTData:
//  def unapply[Z](ast: AST[Z]): Option[FunTData] =
//    ast match
//      case fun_t: FunT => Some(fun_t.as_FunTData)
//      case _ => None
//  end unapply
//end FunTData


type AppT = AST.App0T[?] |
  AST.App1T[?, ?] |
  AST.App2T[?, ?, ?] |
  AST.App3T[?, ?, ?, ?] |
  AST.App4T[?, ?, ?, ?, ?] |
  AST.App5T[?, ?, ?, ?, ?, ?]

case class AppTData(app_t: AppT, f: AST[?], args: ASTs, ty: Ty[?])

extension (app_t: AppT)
  def as_AppTData: AppTData =
    app_t match
      case app_t @ AST.App0T(f,                ty) => AppTData(app_t, f, List(),              ty)
      case app_t @ AST.App1T(f, a,             ty) => AppTData(app_t, f, List(a),             ty)
      case app_t @ AST.App2T(f, a, b,          ty) => AppTData(app_t, f, List(a, b),          ty)
      case app_t @ AST.App3T(f, a, b, c,       ty) => AppTData(app_t, f, List(a, b, c),       ty)
      case app_t @ AST.App4T(f, a, b, c, d,    ty) => AppTData(app_t, f, List(a, b, c, d),    ty)
      case app_t @ AST.App5T(f, a, b, c, d, e, ty) => AppTData(app_t, f, List(a, b, c, d, e), ty)
  end as_AppTData
end extension
