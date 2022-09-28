package trees

enum ArithBinOp:
  case AddOp
  case SubOp
  case MulOp
  case DivOp

  def op_str: String = binop_default_repr(this)
end ArithBinOp

enum ComparisonBinOp:
  case EqOp
  case NotEqOp
  case LtOp
  case LtEqOp
  case GtOp
  case GtEqOp

  def op_str: String = binop_default_repr(this)
end ComparisonBinOp

enum BoolBinOp:
  case OrOp
  case AndOp

  def op_str: String = binop_default_repr(this)

type BinOp  = BoolBinOp | ArithBinOp | ComparisonBinOp

enum BoolUnaryOp:
  case Not

  def op_str: String = unaryop_default_repr(this)
end BoolUnaryOp

type UnaryOp = BoolUnaryOp

// A "default" string representation.
// Since this is used in the Scala generator, if you change this one you must change
// the Scala generator as well.
def binop_default_repr(op: BinOp): String =
  op match
    case ComparisonBinOp.EqOp    => "=="
    case ComparisonBinOp.NotEqOp => "!="
    case ComparisonBinOp.LtOp    => "<"
    case ComparisonBinOp.LtEqOp  => "<="
    case ComparisonBinOp.GtOp    => ">"
    case ComparisonBinOp.GtEqOp  => ">="
    case ArithBinOp.AddOp => "+"
    case ArithBinOp.SubOp => "-"
    case ArithBinOp.MulOp => "*"
    case ArithBinOp.DivOp => "/"
    case BoolBinOp.OrOp  => "||"
    case BoolBinOp.AndOp => "&&"
  end match
end binop_default_repr


// See comments for `binop_default_repr`.
def unaryop_default_repr(op: UnaryOp): String =
  op match
    case BoolUnaryOp.Not => "!"
  end match
end unaryop_default_repr


