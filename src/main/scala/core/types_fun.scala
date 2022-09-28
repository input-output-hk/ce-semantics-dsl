package core

// Type aliases for function types, in order to make life easier for our eyes.
// These are the types of functions as seen from a particular interpretation `R[]`'s point of view.
// See also `Core`, which is parameterized with `R[_]`. 
type Fun0[R[_],                Z] = ()                             => R[Z]
type Fun1[R[_],             A, Z] = (R[A])                         => R[Z]
type Fun2[R[_],          A, B, Z] = (R[A], R[B])                   => R[Z]
type Fun3[R[_],       A, B, C, Z] = (R[A], R[B], R[C])             => R[Z]
type Fun4[R[_],    A, B, C, D, Z] = (R[A], R[B], R[C], R[D])       => R[Z]
type Fun5[R[_], A, B, C, D, E, Z] = (R[A], R[B], R[C], R[D], R[E]) => R[Z]

