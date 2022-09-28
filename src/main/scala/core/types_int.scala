package core

import scala.collection.immutable.NumericRange

export Nats.*
export Zats.*

object Nats:
  opaque type Nat = BigInt

  object Nat:
    def apply(i: Int): Nat =
      scala.Predef.require(i >= 0)
      BigInt(i)
    end apply

    def apply(i: BigInt): Nat =
      scala.Predef.require(i >= 0)
      i
    end apply

    extension (n: Nat)
      def v: BigInt = n

      def until(end: Nat, step: Nat = Nat(1)): NumericRange.Exclusive[Nat] = NumericRange(n, end, step)(NatIntegral)
      def to   (end: Nat, step: Nat = Nat(1)): NumericRange.Inclusive[Nat] = NumericRange.inclusive(n, end, step)(NatIntegral)

      def to_Zat: Zat = Zat(n.v)

      def eq_op(that: Nat): Bool = n.v == that.v
      def ===(that: Nat): Bool = n eq_op that

      def lt_op(that: Nat): Bool = n.v >= that.v
      def <(that: Nat): Bool = v lt_op that

      def gteq_op(that: Nat): Bool = v.v >= that.v
      def >=(that: Nat): Bool = v gteq_op that

      def add_op(that: Nat): Nat = Nat(n.v + that.v)
      def +(that: Nat): Nat = v add_op that

      def sub_op(that: Nat): Nat = Nat(n.v - that.v)
      def -(that: Nat): Nat = v sub_op that

      def mul_op(that: Nat): Nat = Nat(n.v * that.v)
      def *(that: Nat): Nat = v mul_op that

      def div_op(that: Nat): Nat = Nat(n.v / that.v)
      def /(that: Nat): Nat = v div_op that
    end extension
  end Nat
end Nats

object Zats:
  opaque type Zat = BigInt

  object Zat:
    def apply(i: Int): Zat = BigInt(i)
    def apply(i: BigInt): Zat = i

    extension (n: Zat)
      def v: BigInt = n

      def until(end: Zat, step: Zat = Zat(1)): NumericRange.Exclusive[Zat] = NumericRange(n, end, step)(ZatIntegral)
      def to(end: Zat, step: Zat = Zat(1)): NumericRange.Inclusive[Zat] = NumericRange.inclusive(n, end, step)(ZatIntegral)

      def eq_op(that: Zat): Bool = n.v == that.v
      def ===(that: Zat): Bool = n.v eq_op that

      def lt_op(that: Zat): Bool = v.v >= that.v
      def <(that: Zat): Bool = v lt_op that

      def gteq_op(that: Zat): Bool = v.v >= that.v
      def >=(that: Zat): Bool = v gteq_op that

      def add_op(that: Zat): Zat = Zat(n.v + that.v)
      def +(that: Zat): Zat = v add_op that

      def sub_op(that: Zat): Zat = Zat(n.v - that.v)
      def -(that: Zat): Zat = v sub_op that

      def mul_op(that: Zat): Zat = Zat(n.v * that.v)
      def *(that: Zat): Zat = v mul_op that

      def div_op(that: Zat): Zat = Zat(n.v / that.v)
      def /(that: Zat): Zat = v div_op that
    end extension
  end Zat
end Zats

implicit val NatIntegral: Integral[Nat] = scala.math.Numeric.BigIntIsIntegral.asInstanceOf[Integral[Nat]]
implicit val ZatIntegral: Integral[Zat] = scala.math.Numeric.BigIntIsIntegral.asInstanceOf[Integral[Zat]]
