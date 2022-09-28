package core

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

// Core extended to aid in property checking.
trait PropCore[I[_]] extends Core[I]:
  // https://www.scalatest.org/user_guide/property_based_testing
  // https://github.com/typelevel/scalacheck/blob/main/doc/UserGuide.md
  
  given Arb_I_Nat: Arbitrary[I[Nat]] = Arbitrary(Gen.resultOf(nat))
  given Arb_I_Zat: Arbitrary[I[Zat]] = Arbitrary(Gen.resultOf(zat(_: Int)))
  given Arb_I_Bool: Arbitrary[I[Bool]] = Arbitrary(Gen.resultOf(bool))
  given Arb_I_String: Arbitrary[I[String]] = Arbitrary(Gen.resultOf(str))
  given Arb_I_Char: Arbitrary[I[Char]] = Arbitrary(Gen.resultOf(char))
end PropCore

abstract class ScalaCheckPropertyChecks[I[_]](core: PropCore[I]) extends org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
