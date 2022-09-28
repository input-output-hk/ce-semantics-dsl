package core

import scala.reflect.ClassTag

// Algorithmic type construction rules, encoded in the logic of Scala implicits.
// We follow what the compiler does but at the value level, so that these can be manipulated
// and introspected directly by the user (=programmer).
// `R[_]` is our semantics interpretation domain.
trait TypeInference[R[_]]:
  given given_Unit  : Ty[Unit] = Ty.UnitTy
  given given_NatTy : Ty[Nat]  = Ty.NatTy
  given given_ZatTy : Ty[Zat]  = Ty.ZatTy
  given given_BoolTy: Ty[Bool] = Ty.BoolTy
  given given_StringTy: Ty[String] = Ty.StringTy
  given given_CharTy: Ty[Char] = Ty.CharTy

  given given_RTy[X](using x_ty: Ty[X]): Ty[R[X]] = Ty.RTy(x_ty)

  given given_PropTy: Ty[Prop[R]] = Ty.PropTy[R]()

  given given_Fun0Ty[Z      ]         (using z: Ty[Z]): Ty[() => Z] = Ty.Fun0Ty(z)
  given given_Fun1Ty[A, Z   ]         (using a: Ty[A], z: Ty[Z]): Ty[(A) => Z] = Ty.Fun1Ty(a, z)
  given given_Fun2Ty[A, B, Z]         (using a: Ty[A], b: Ty[B], z: Ty[Z]): Ty[(A, B) => Z] = Ty.Fun2Ty(a, b, z)
  given given_Fun3Ty[A, B, C, Z]      (using a: Ty[A], b: Ty[B], c: Ty[C], z: Ty[Z]): Ty[(A, B, C) => Z] = Ty.Fun3Ty(a, b, c, z)
  given given_Fun4Ty[A, B, C, D, Z]   (using a: Ty[A], b: Ty[B], c: Ty[C], d: Ty[D], z: Ty[Z]): Ty[(A, B, C, D) => Z] = Ty.Fun4Ty(a, b, c, d, z)
  given given_Fun5Ty[A, B, C, D, E, Z](using a: Ty[A], b: Ty[B], c: Ty[C], d: Ty[D], e: Ty[E], z: Ty[Z]): Ty[(A, B, C, D, E) => Z] = Ty.Fun5Ty(a, b, c, d, e, z)

  given given_OptionTy[A]   (using a: Ty[A]          ): Ty[Option[A]]    = Ty.OptionTy(a)
  given given_PairTy  [A, B](using a: Ty[A], b: Ty[B]): Ty[Pair[A, B]]   = Ty.PairTy(a, b)

  given given_MapTy[A, B](using a: Ty[A], b: Ty[B]): Ty[Map[A, B]] = Ty.MapTy(a, b)
  given given_SetTy[A](using a: Ty[A]): Ty[Set[A]] = Ty.SetTy(a)
  given given_SeqTy[A](using a: Ty[A]): Ty[Seq[A]] = Ty.SeqTy(a)

  given [S <: AnyStruct](using tag: ClassTag[S]): Ty[S] = Ty.StructTy(tag)
end TypeInference
