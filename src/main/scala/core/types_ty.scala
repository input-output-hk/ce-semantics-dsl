package core

import trees.cgtree.StdCodeGenBase // Note I do not like this import (in terms of package dependencies).
import scala.annotation.targetName
import scala.reflect.ClassTag

enum Ty[T]:
  case UnitTy   extends Ty[Unit]
  case NatTy    extends Ty[Nat]
  case ZatTy    extends Ty[Zat]
  case BoolTy   extends Ty[Bool]
  case StringTy extends Ty[String]
  case CharTy   extends Ty[Char]

  case RTy[X, R[_]](inner: Ty[X]) extends Ty[R[X]]

  case PropTy[R[_]]() extends Ty[Prop[R]] // for pre-, post- conditions and other formal properties/statements

  case Fun0Ty[Z]               (z: Ty[Z])                                                   extends Ty[() => Z]
  case Fun1Ty[A, Z]            (a: Ty[A], z: Ty[Z])                                         extends Ty[(A) => Z]
  case Fun2Ty[A, B, Z]         (a: Ty[A], b: Ty[B], z: Ty[Z])                               extends Ty[(A, B) => Z]
  case Fun3Ty[A, B, C, Z]      (a: Ty[A], b: Ty[B], c: Ty[C], z: Ty[Z])                     extends Ty[(A, B, C) => Z]
  case Fun4Ty[A, B, C, D, Z]   (a: Ty[A], b: Ty[B], c: Ty[C], d: Ty[D], z: Ty[Z])           extends Ty[(A, B, C, D) => Z]
  case Fun5Ty[A, B, C, D, E, Z](a: Ty[A], b: Ty[B], c: Ty[C], d: Ty[D], e: Ty[E], z: Ty[Z]) extends Ty[(A, B, C, D, E) => Z]

  case OptionTy[A](a: Ty[A]) extends Ty[Option[A]]
  case PairTy[A, B](a: Ty[A], b: Ty[B]) extends Ty[Pair[A, B]]

  case MapTy[A, B](a: Ty[A], b: Ty[B]) extends Ty[Map[A, B]]
  case SetTy[A](a: Ty[A]) extends Ty[Set[A]]
  case SeqTy[A](a: Ty[A]) extends Ty[Seq[A]]

  case StructTy[Struct <: AnyStruct](tag: ClassTag[Struct]) extends Ty[Struct]

  // Removes the I[_] tag
  def untag: Ty[_] = this match
    case Ty.RTy(inner) => inner.untag

    case Ty.Fun0Ty(z) => Ty.Fun0Ty(z.untag)
    case Ty.Fun1Ty(a, z) => Ty.Fun1Ty(a.untag, z.untag)
    case Ty.Fun2Ty(a, b, z) => Ty.Fun2Ty(a.untag, b.untag, z.untag)
    case Ty.Fun3Ty(a, b, c, z) => Ty.Fun3Ty(a.untag, b.untag, c.untag, z.untag)
    case Ty.Fun4Ty(a, b, c, d, z) => Ty.Fun4Ty(a.untag, b.untag, c.untag, d.untag, z.untag)
    case Ty.Fun5Ty(a, b, c, d, e, z) => Ty.Fun5Ty(a.untag, b.untag, c.untag, d.untag, e.untag, z.untag)

    case Ty.OptionTy(a)    => Ty.OptionTy(a.untag)
    case Ty.PairTy(a, b)   => Ty.PairTy(a.untag, b.untag)

    case Ty.MapTy(a, b) => Ty.MapTy(a.untag, b.untag)
    case Ty.SetTy(a)    => Ty.SetTy(a.untag)
    case Ty.SeqTy(a)    => Ty.SeqTy(a.untag)

    case _ => this
  end untag

  // reference string representation of a Ty.
  def repr: String = StdCodeGenBase.ty_to_str(this)
end Ty

type Tys = List[Ty[?]]

