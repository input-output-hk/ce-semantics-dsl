package examples
package indian

import core.*

trait Curry[R[_]] extends Core[R]:
  import TypeInference.given

  def fun2_curried[A: Ty, B: Ty, C: Ty](
    x: Name, y: Name,
    f: (R[A], R[B]) => R[C]
  ): R[Fun1[R, A, Fun1[R, B, C]]] =
  
    fun1(x, (x: R[A]) => fun1(y, (y: R[B]) => f(x, y)))
  end fun2_curried

  def fun3_curried[A: Ty, B: Ty, C: Ty, D: Ty](
    x: Name, y: Name, z: Name,
    f: (R[A], R[B], R[C]) => R[D]
  ): R[Fun1[R, A, Fun1[R, B, Fun1[R, C, D]]]] =

    fun1(x, (x: R[A]) => fun2_curried(y, z, (y: R[B], z: R[C]) => f(x, y, z)))
  end fun3_curried
end Curry
