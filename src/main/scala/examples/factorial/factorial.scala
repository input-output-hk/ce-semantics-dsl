package examples
package factorial

import core.*
import chain.*
import chain.demo.*
import trees.*
import trees.ast.*
import trees.*
import trees.cgtree.*

trait Factorial[R[_]] extends Core[R]:
  import TypeInference.given
  
  // factorial in "normal" Scala
  object Warmup:
    def fact(n: Int): Int =
      require(n >= 0)
      
      if n == 0 then 1 else n * fact(n - 1)
    end fact
  end Warmup
  
  def impl_fact(n: R[Nat]): R[Nat] =
    // `[Nat]` is needed to aid type inference.
    // Otherwise, we would need to specify `N_1` instead of the first `1`.
    if_then_else[Nat](n === 0)(1)(n * fact(n - 1))
  end impl_fact

  lazy val fact = f["fact"] := (p["n"]) --> impl_fact
end Factorial


@main def factorial_demo(): Unit =
  val factorial_ast    = new ASTCore    with Factorial[AST]
  val factorial_direct = new DirectCore with Factorial[Id]

  def do_factorial_ast(): Unit =
    val code = gen_code(
      List(
        factorial_ast.N_0,
        factorial_ast.N_1,
        factorial_ast.fact,
      )
    )

    println(code)
  end do_factorial_ast

  def do_factorial_direct(): Unit =
    import factorial_direct._
    println()
    val n = nat(5)
    val fact_n = fact(n)

    println(s"n = ${n}")
    println(s"fact(n) = ${fact_n}")
  end do_factorial_direct

  do_factorial_direct()
  do_factorial_ast()
end factorial_demo
