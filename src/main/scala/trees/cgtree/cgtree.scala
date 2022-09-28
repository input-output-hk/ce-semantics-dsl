package trees
package cgtree

import core.*
import trees.ast.*
import trees.ast.AST.*
import trees.cgtree.CGTree.FunDefT

import java.util.Locale

// Helper struct
case class NameType(name: String, ty: Ty[?])
type NameTypes = List[NameType]

// Helper struct
case class NameCGTree(name: String, value: CGTree)
type NameCGTrees = List[NameCGTree]

// Marker traits, just for documentation at the moment.
sealed trait ExprTree   // Expressions
sealed trait DefTree    // Definitions
sealed trait PrimTree   // Primitives, which means they are provided be the native code generator, not by user code.

enum Literal:
  case BoolLit  (value: Bool)
  case NatLit   (value: Nat)
  case ZatLit   (value: Zat)
  case StringLit(value: String)
  case CharLit  (value: Char)
end Literal

/**
 * See also [[core.Prop]]. This is its elaboration, used for code-generation.
 *
 * Note Revisit this design when for-loops are introduced in the eDSL and AST.
 */
enum CGProp:
  case BoolCGP(
    prop_is: PropIs,
    prop_owner_name: String, // e.g. the function for which this property is defined
    prop_name: String,
    prop_index: Int, // index of the property among the owner's other properties of the same kind
    prop_input: NameTypes,
    predicate: CGTrees
  )

  case ForAllInRangeCGP(
    prop_is: PropIs,
    prop_owner_name: String, // e.g. the function for which this property is defined
    prop_name: String, // synthetic name for the property
    prop_index: Int, // index of the property among the owner's other properties of the same kind
    prop_input: NameTypes,
    for_loop_deps: CGTrees,    // code that should be emitted before the for loop is emitted
    for_loop: CGTree.ForLoopT  // code that is emitted in the for loop
  )
end CGProp

enum InlineIs:
  case Inline
  case NotInline
end InlineIs

// "Code Generation Tree", meaning we are ready to generate code from this representation.
// TODO preserve the type for each CGTree node. Just copy the proper `Ty[?]` value from the AST.
enum CGTree:
  // Expressions (user-defined)
  case LiteralExprT   (value: Literal)                              extends CGTree with ExprTree
  case RefExprT       (bind_t: AST.BindT[?])                        extends CGTree with ExprTree // TODO Do we really need a BindT here or just a name and type?
  case BinOpExprT     (op: BinOp,   a: CGTree, b: CGTree)           extends CGTree with ExprTree
  case UnaryOpExprT   (op: UnaryOp, a: CGTree)                      extends CGTree with ExprTree
  case CallExprT      (f: CGTree, types: Tys, args: CGTrees)        extends CGTree with ExprTree
  case StructNewExprT (name: String, fields: NameCGTrees)           extends CGTree with ExprTree
  case DotExprT       (prefix: CGTree, field: NameType)             extends CGTree with ExprTree
  case IfThenElseExprT(_if: CGTree, _then: CGTree, _else: CGTree) extends CGTree with ExprTree

  // Definitions (user-defined)
  case StructDefT   (name: String, fields: NameTypes)                               extends CGTree with DefTree // case class Name(a: A, ...)

  case ValDefT      (name_ty: NameType, value: CGTree, is_lazy: Bool)               extends CGTree with DefTree // val x: T = ...
  case FunDefT(
    name: String,
    params: NameTypes,
    ret_ty: Ty[?],
    body: CGTrees,
    inline_is: InlineIs,
    pre_cond: List[CGProp],  // function running pre-conditions
    post_cond: List[CGProp]  // function running post-conditions
  ) extends CGTree with DefTree // def f(a: A, ...): R = ...

  case LambdaDefT(
    params: NameTypes,
    ret_ty: Ty[?],
    body: CGTrees
  ) extends CGTree with DefTree

  // Primitive code of any kind: imports, types, data, functions, ...
  // Each code generator is responsible for providing correct code in the given string.
  // For instance, Scala code could be:
  //  type Seq[K] = scala.collection.immutable.IndexedSeq[K]
  case PrimCodeT(code: String) extends CGTree with DefTree with PrimTree

  // Other statements
  case CommentT(comment: String) // comment like this one
  case ForLoopT(
    name: String,
    lower: CGTree,
    upper: CGTree,
    upper_bound_is: BoundIs,
    body: CGTrees
  )

  // Properties
  case RequireT(tree: CGTree) // tree is a Boolean expression
  case EnsureT(tree: CGTree)  // tree is a Boolean expression
end CGTree

type CGTrees = List[CGTree]
type Fragment = CGTree // TODO Now that the exact type is settled (CGTree), remove `Fragment` and use `CGTree` directly.

extension (tree: CGTree)
  def to_prop_statement(prop_is: PropIs): CGTree =
    tree match
      case CGTree.RequireT(_) | CGTree.EnsureT(_) => tree
      case _ =>
        prop_is match
          case PropIs.PreCondition  => CGTree.RequireT(tree)
          case PropIs.PostCondition => CGTree.EnsureT(tree)
        end match
    end match
  end to_prop_statement
end extension
