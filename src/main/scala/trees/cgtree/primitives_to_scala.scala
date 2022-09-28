package trees
package cgtree

// Scala code generator of primitives
class PrimFunToScala extends PrimFunVisitor:
  def visit_Map_New(pf: PrimFun, ctx: Ctx): Result =
    // The Map used here is defined elsewhere in the preamble
    ctx.write_code_ln(s"inline def ${pf.identifier}[K, V](): Map[K, V] = Map()")
  end visit_Map_New

  def visit_Map_Get(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}[K, V](map: Map[K, V], key: K): V = map(key)")
  end visit_Map_Get

  def visit_Map_Put(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}[K, V](map: Map[K, V], key: K, value: V): Map[K, V] = map.updated(key, value)")
  end visit_Map_Put

  def visit_Map_Values(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}[K, V](map: Map[K, V]): Seq[V] = map.values.toIndexedSeq")
  end visit_Map_Values

  def visit_Set_New(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}[K](): Set[K] = Set()")
  end visit_Set_New

  def visit_Set_Add(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}[K](set: Set[K], item: K): Set[K] = set + item")
  end visit_Set_Add

  def visit_Seq_New(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}[K](items: K*): Seq[K] = Seq(items*)")
  end visit_Seq_New

  def visit_Seq_Add(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}[K](seq: Seq[K], item: K): Seq[K] = seq appended item")
  end visit_Seq_Add

  def visit_Seq_Map(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}[K, L](seq: Seq[K], f: (K) => L): Seq[L] = seq map f")
  end visit_Seq_Map

  def visit_Seq_Fold_Left(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}[K, L](seq: Seq[K], initial: L, f: (L, K) => L): L = seq.foldLeft[L](initial)(f(_, _))")
  end visit_Seq_Fold_Left

  def visit_Nat(pf: PrimFun, ctx: Ctx): Result =
    def do_visit(tpe: String): Result =
      ctx.write_code_ln(s"inline def ${pf.identifier}(i: ${tpe}): Nat =")
      ctx.with_indentation(Indentation.More) {
        ctx.write_code_ln("scala.Predef.require(i >= 0)")
        ctx.write_code_ln("Nat(i)")
      }
      ctx.write_code_ln(s"end ${pf.identifier}")
    end do_visit

    do_visit("scala.Int")
    ctx.write_ln()
    do_visit("scala.BigInt")
  end visit_Nat

  def visit_Zat(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}(i: scala.Int   ): Zat = Zat(i)")
    ctx.write_code_ln(s"inline def ${pf.identifier}(i: scala.BigInt): Zat = Zat(i)")
  end visit_Zat

  def visit_Zat_Of_Nat(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}(nat: Nat): Zat = nat.to_Zat")
  end visit_Zat_Of_Nat

  def visit_Str_Len(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}(s: String): Nat = nat(s.length)")
  end visit_Str_Len

  def visit_Str_Char_At(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}(s: String, i: Nat): Char = s.charAt(i.v.intValue)")
  end visit_Str_Char_At

  def visit_Str_Concat(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}(a: String, b: String): String = a + b")
  end visit_Str_Concat

  def visit_Char(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}(c: scala.Char): Char = c")
  end visit_Char

  def visit_Char_To_Str(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}(c: scala.Char): String = c.toString")
  end visit_Char_To_Str

  def visit_Char_Eq(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}(a: scala.Char, b: scala.Char): Boolean = a == b")
  end visit_Char_Eq

  def visit_Pair_New(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}[A, B](a: A, b: B): Pair[A, B] = (a, b)")
  end visit_Pair_New

  def visit_Pair_First(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}[A, B](pair: Pair[A, B]): A = pair._1")
  end visit_Pair_First

  def visit_Pair_Second(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}[A, B](pair: Pair[A, B]): B = pair._2")
  end visit_Pair_Second

  def visit_Require(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}[A](condition: Boolean, message: =>A): Unit = scala.Predef.require(condition, message)")
  end visit_Require

  def visit_Ensure(pf: PrimFun, ctx: Ctx): Result =
    ctx.write_code_ln(s"inline def ${pf.identifier}[A](condition: Boolean, message: =>A): Unit = scala.Predef.assert(condition, message)")
  end visit_Ensure
end PrimFunToScala

