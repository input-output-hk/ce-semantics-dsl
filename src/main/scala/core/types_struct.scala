package core

type FieldName = Name /*& Singleton*/

// Marker trait for user-defined structs
trait AnyStruct extends Product

// Helper class to support user-defined structs (records, case classes, ...)
case class StructDef[ Struct <: AnyStruct,
  Fields <: Tuple, // definition of fields, as obtained from new_bare_field[]
  Args   <: Tuple  // constructor, as obtained from struct_constructor[]
](name: Name, ty: Ty[Struct], fields: Fields, constr: Args => Struct)

type StructDefs = List[StructDef[?, ?, ?]]

case class FieldDef[Struct <: AnyStruct,
  N <: FieldName,
  F,
  R[_]
](struct: StructDef[Struct, _ <: Tuple, _ <: Tuple],
  field: BareField[N, F],
  getter: Struct => R[F]
)

// A bare field lives outside the definition of any Struct,
// so that it can be re-used.
case class BareField[N <: FieldName, F](name: N, ty: Ty[F])
