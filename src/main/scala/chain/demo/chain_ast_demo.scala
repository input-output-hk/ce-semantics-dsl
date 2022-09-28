package chain
package demo

import core.*
import trees.ast.*
import trees.ast.AST.*

object ASTChainWithDemo extends ASTCore with ChainWithDemoConfig[AST]:
  import TypeInference.given

  val One  = bind(name("One"), N_1)
  val Zero = bind(name("Zero"), N_0)
  val One_plus_Zero = bind(name("one_plus_zero"), One + Zero)

  def impl_add(x: AST[Nat], y: AST[Nat]): AST[Nat] =
    //val tmp = "tmp" := y + One
    x + /*tmp +*/ y
  end impl_add
  val add = bind(name("add"), fun2(name("x"), name("y"), impl_add))
  val add_ = bind("add", fun2("x", "y", impl_add))

  val add_1 = bind(name("add_1"), fun1("x", (x: AST[Nat]) => x + One))

  def impl_foobar(): AST[Nat] =
    One + Zero
  end impl_foobar
  val foobar = bind("foobar", fun0(impl_foobar))

  def impl_foobar2(): AST[Nat] =
    add_1(Zero + add_1(One))
  end impl_foobar2
  val foobar2 = bind("foobar2", fun0(impl_foobar2))
  val foobar3 = bind("foobar3", app0(foobar2))

  def impl_test_bool_or(a: AST[Bool]): AST[Bool] =
    val b = bind(name("b"), bool(true))
    val m: AST[Nat] = bind(name("m"), N_1)
    val test_m = bind(name("test_m"), m === N_1)

    a || test_m || b
  end impl_test_bool_or
  val test_bool_or = bind(name("test_bool_or"), fun1(name("a"), impl_test_bool_or))
end ASTChainWithDemo
