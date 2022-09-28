package examples
package leftpad

import core.*
import chain.*
import chain.demo.*
import trees.*
import trees.ast.*
import trees.*
import trees.cgtree.*

import scala.annotation.tailrec

trait LeftPad[R[_]] extends PropCore[R]:
  import TypeInference.given

  object Warmup:
    // So this is just a straightforward recursive implementation.
    @tailrec
    def leftpad(s: String, n: Int, c: Char): String =
      require(n >= 0)

      if s.length >= n then s else leftpad(c.toString ++ s, n, c)
    end leftpad

  // And this is an implementation based on the Core eDSL.
  def impl_leftpad(s: R[String], n: R[Nat], c: R[Char]): R[String] =
    if_then_else( s.length >= n ) {
      s
    }{
      val cs = v["cs"] := c.to_str
      val ps = v["ps"] := cs ++ s

      leftpad(ps, n, c)

      // Other, more direct in terms of syntax, approaches using existing facilities in the eDSL:
      //   leftpad(c.to_str ++ s, n, c)
      // or:
      //   leftpad(str_concat(char_to_str(c), s), n, c)
    }
  end impl_leftpad

  // shorter version
  def impl_leftpad_(s: R[String], n: R[Nat], c: R[Char]): R[String] =
    if_then_else( s.length >= n ) {
      s
    } {
      leftpad(c.to_str ++ s, n, c)
    }
  end impl_leftpad_

  // helper definitions
  val params      = (p["s"], p["n"], p["c"])
  val params_pre  = params
  val params_post = params ++ Tuple1(p["result"]) // A post-condition needs one extra param, the result
  lazy val leftpad =
      ( f["leftpad"] := params --> impl_leftpad ).
        add_precond3 ( params_pre  --> impl_leftpad_precond1 ).
        add_postcond3( params_post --> impl_leftpad_postcond1 ).
        add_postcond3( params_post --> impl_leftpad_postcond2 ).
        add_postcond3( params_post --> impl_leftpad_postcond3 )

  // Pre-conditions for leftpad.
  // Some DSLs use the `requires` syntactic form.
  def impl_leftpad_precond1(s: R[String], n: R[Nat], c: R[Char]): R[Prop[R]] =
    // These vals are here mostly to test the code generator
    val n_lower_value = v["n_lower_value"] := nat(0)
    val n_upper_value = v["n_upper_value"] := n

    // $ n \geqslant 0 $
    prop_bool(n_upper_value >= n_lower_value)
  end impl_leftpad_precond1

  // short version of precond 1
  def precond1(s: R[String], n: R[Nat], c: R[Char]): R[Prop[R]] =
    // $ n \geqslant 0 $
    prop_bool( n >= nat(0) )
  end precond1

  // Post-conditions for leftpad.
  // Some DSLs use the `ensures` syntactic form.
  def impl_leftpad_postcond1(s: R[String], n: R[Nat], c: R[Char], result: R[String]): R[Prop[R]] =
    // These vals are here mostly to test the code generator
    val res_length = v["res_length"] := result.length
    val s_length   = v["s_length"]   := s.length

    // $ length(result) = max(n, length(s)) $
    prop_bool(res_length === max(n, s_length))
  end impl_leftpad_postcond1

  // short version of postcond 1
  def postcond1(s: R[String], n: R[Nat], c: R[Char], result: R[String]): R[Prop[R]] =
    // $ length(result) = max(n, length(s)) $
    prop_bool( result.length === max(n, s.length) )
  end postcond1

  def impl_leftpad_postcond2(s: R[String], n: R[Nat], c: R[Char], result: R[String]): R[Prop[R]] =
    // These vals are here mostly to test the code generator
    val pad_length = v["pad_length"] := max(0, n - s.length)
    val lower      = v["lower"]      := nat(0)
    val upper      = v["upper"]      := pad_length // Note (code generator) that we use `pad_length` outside the for loop

    // $ \forall i \in [0, N), N = max(0, n - length(s)) \implies result[i] = c $
    prop_forall_in["i", Nat](
      (lower, upper),
      UpperBoundIsExclusive,
      (i) => {
        // Note (code generator) This should not escape the lexical scope, since it depends on `i`.
        val result_i = v["result_i"] := result(i)

        c === result_i
      }
    )
  end impl_leftpad_postcond2

  // short version of postcond 2
  def postcond2(s: R[String], n: R[Nat], c: R[Char], result: R[String]): R[Prop[R]] =
    // $ \forall i \in [0, N), N = max(0, n - length(s)) \implies result[i] = c $
    prop_forall_in["i", Nat](
      (nat(0), max(0, n - s.length)),
      UpperBoundIsExclusive,
      (i) => c === result(i)
    )
  end postcond2

  def impl_leftpad_postcond3(s: R[String], n: R[Nat], c: R[Char], result: R[String]): R[Prop[R]] =
    // These vals are here mostly to test the code generator
    val pad_length = v["pad_length"] := max(0, n - s.length)
    val lower = v["lower"] := nat(0)
    val upper = v["upper"] := s.length

    prop_forall_in["i", Nat](
      (lower, upper),
      UpperBoundIsExclusive,
      (i) => {
        // Note (code generator) This should not escape the lexical scope, since it depends on `i`.
        val index = v["index"] := i

        result(pad_length + i) === s(index) // Note (code generator) that we use `pad_length` inside the for loop
      }
    )
  end impl_leftpad_postcond3

  // Shown as an example of code to be auto-generated, once we have
  // the definitions of a function and its pre-/post-conditions.
  def impl_leftpad_spec(s: R[String], n: R[Nat], c: R[Char]): R[String] =
    // post-preconditions
    impl_leftpad_precond1(s, n, c)

    // computation
    val result = impl_leftpad(s, n, c)

    // post-conditions
    impl_leftpad_postcond1(s, n, c, result)
    impl_leftpad_postcond2(s, n, c, result)
    impl_leftpad_postcond3(s, n, c, result)

    result
  end impl_leftpad_spec
end LeftPad

@main def leftpad_demo(): Unit =
  val leftpad_ast    = new ASTCore    with LeftPad[AST]
  val leftpad_direct = new DirectCore with LeftPad[Id]

  def do_leftpad_ast(): Unit =
    val code = gen_code(
      leftpad_ast.Utility_Defs ++
      List(
        leftpad_ast.leftpad,
      )
    )

    println(code)
  end do_leftpad_ast

  def do_leftpad_direct(): Unit =
    val s = "help"
    val n = 6
    val c = 'X'
    println()
    println(s"s = ${s}, s.length = ${s.length}, n = ${n}, c = '${c}'")
    val lpad_s = {
      import leftpad_direct._
      leftpad(str(s), nat(n), char(c))
    }
    println(s"lpad_s = ${lpad_s}")
  end do_leftpad_direct

  //leftpad_ast.Tests.check_pre()

  do_leftpad_direct()
  do_leftpad_ast()
end leftpad_demo
