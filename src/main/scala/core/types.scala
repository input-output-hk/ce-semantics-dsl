package core

type Bool   = scala.Boolean
type String = scala.Predef.String
type Unit   = scala.Unit
type Char   = scala.Char

// an identifier in the object language for which we require a literal string
type Name = String & Singleton

type Option[T]    = scala.Option[T]

type Pair[A, B] = (A, B)
type Map[K, V] = scala.collection.immutable.Map[K, V]
type Set[K] = scala.collection.immutable.Set[K]
type Seq[K] = scala.collection.immutable.IndexedSeq[K]

val Map = scala.collection.immutable.Map
val Set = scala.collection.immutable.Set
val Seq = scala.collection.immutable.IndexedSeq
