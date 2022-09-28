package examples

import trees.{CodeGenHelpers, SourceCodeWriter}
import trees.ast.*
import trees.cgtree.CGTreeToScala

def gen_module(bindings: BindTs): SourceCodeWriter =
  CodeGenHelpers.gen_module(List(), bindings, new CGTreeToScala())
end gen_module

def gen_code(bindings: BindTs): String =
  gen_module(bindings.toList).get_code()
end gen_code
