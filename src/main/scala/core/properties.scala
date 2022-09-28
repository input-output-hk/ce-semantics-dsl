package core

import trees.cgtree.NameType

enum PropIs:
  /**
   * A pre-condition, used to check (function) input. Used in a function, it signifies a caller's responsibility.
   * You can usually recognize a pre-condition by the use of `requires` in code.
   */
  case PreCondition

  /**
   * A post-condition, used to check the result of a computation, e.g. the function output.
   * You can usually recognize a post-condition by the use of `ensures` or `ensuring` in code.
   */
  case PostCondition
  
  // case Invariant
end PropIs

extension (prop_is: PropIs)
  // Used in code generation
  def source_code_identifier: String =
    prop_is match
      case PropIs.PreCondition  => "pre_cond"
      case PropIs.PostCondition => "post_cond"
    end match
  end source_code_identifier
  
  // Used in messages
  def label: String =
    prop_is match
      case PropIs.PreCondition => "Pre"
      case PropIs.PostCondition => "Post"
    end match
  end label
end extension

enum BoundIs:
  case Inclusive
  case Exclusive
end BoundIs

val UpperBoundIsExclusive = BoundIs.Exclusive
val UpperBoundIsInclusive = BoundIs.Inclusive

/**
 * Property to be checked mechanically/automatically.
 * The role of `R[]` is the same as in the definition of [[Core]].
 */
enum Prop[R[_]]:
  // Simple boolean property
  case BoolP(predicate: R[Bool])
  
  // This is a special case of a universal quantifier with constraints.
  // The lower bound (first in the tuple) is always inclusive, while the upper bound can be either inclusive or exclusive.
  case ForAllInRangeP[X <: (Nat | Zat), R[_]](
    name: Name,
    ty: Ty[X],
    bounds: (R[X], R[X]),
    upper_bound_is: BoundIs,
    predicate: R[Fun1[R, X, Bool]]
  ) extends Prop[R]

  // And this can be the more general type.
  //case ForAllP[X <: (Nat | Zat), R[_]](
  //  name: Name,
  //  ty: Ty[X],
  //  constraint: R[Fun1[R, X, Bool]],
  //  predicate: R[Fun1[R, X, Bool]]
  //) extends Prop[R]
end Prop
