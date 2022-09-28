package core

import scala.annotation.targetName

// This is just `|>` from https://github.com/loverdos/mlpipe, adapted to Scala 3
extension [T](x: T)
  @targetName("ext_mlpipe")
  inline def |>[B](f: (T) => B): B = f(x)
end extension
