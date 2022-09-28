package examples
package vanilla

import core.*
import chain.*
import chain.demo.*
import trees.*
import trees.ast.*
import trees.*
import trees.cgtree.*

// Just vanilla examples.
trait Vanilla[R[_]] extends Core[R]:
  import TypeInference.given

  def impl_self_call(n: R[Nat]): R[Nat] =
    val one   = name["one"]   := nat(1)
    val two   = name["two"]   := one + one
    val three = name["three"] := two + two

    // This obviously diverges.
    self_call(one + three)
  end impl_self_call

  lazy val self_call = f["self_call"] := (p["n"]) --> impl_self_call
  
  def impl_triangle(a: R[Nat], b: R[Nat]): R[Nat] =
    a * a + b * b
  end impl_triangle
  
  lazy val triangle = f["triangle"] := (p["a"], p["b"]) --> impl_triangle
  
end Vanilla


@main def vanilla_demo(): Unit =
  val vanilla_ast = new ASTCore with Vanilla[AST]

  def do_vanilla_ast(): Unit =
    val code = gen_code(
      List(
        vanilla_ast.N_0,
        vanilla_ast.N_1,
        vanilla_ast.self_call,
      )
    )

    println(code)
  end do_vanilla_ast
  
  do_vanilla_ast()
end vanilla_demo
