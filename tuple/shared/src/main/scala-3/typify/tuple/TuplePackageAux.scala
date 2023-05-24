package typify
package tuple

private[typify] trait TuplePackageAux

private[typify] final type Tuple = scala.Tuple
private[typify] final type *:[H, T <: Tuple] = scala.*:[H, T]
private[typify] final type EmptyTuple = scala.EmptyTuple
private[typify] final val EmptyTuple: EmptyTuple = scala.EmptyTuple
