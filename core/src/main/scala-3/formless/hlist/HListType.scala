package formless.hlist

/**
 * Match type that will only reduce if `A` is a subtype of `HList`.
 */
type HListType[A] <: HList = A match {
  case HNil => HNil
  case h :: t => h :: t
}
