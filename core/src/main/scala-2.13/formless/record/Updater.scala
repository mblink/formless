package formless.record

import formless.hlist.{DepFn2, HList}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/** Type class supporting record update and extension. */
@scala.annotation.implicitNotFound("Cannot update record ${T} with field ${F}: the key may already have a field with a different value type or a strict subtype of the requested field, or the lookup could be undecidable. To change the field's type, use updateWith or remove it first.")
trait Updater[T <: HList, F] extends DepFn2[T, F] with Serializable {
  type Out <: HList
}

object Updater {
  type Aux[T <: HList, F, O <: HList] = Updater[T, F] { type Out = O }

  def apply[T <: HList, F](implicit u: Updater[T, F]): Aux[T, F, u.Out] = u
  def apply[T <: HList, F](t: T, f: F)(implicit u: Updater[T, F]): u.Out = u(t, f)

  implicit def updaterAt[T <: HList, F, O <: HList]: Aux[T, F, O] = macro Macros.instImpl[T, F]

  private sealed trait Refusal { def message: String }
  private final case class ExistingKey(key: String) extends Refusal {
    def message: String =
      s"Field $key already exists with a different or narrower type. Use updateWith($key)(...) to change its type, or remove it first."
  }
  private final case class Undecided(element: String) extends Refusal {
    def message: String = s"Element $element might have the requested key."
  }

  final class Macros(val c: whitebox.Context) extends shapeless.CaseClassMacros {
    import c.universe._

    private def key(tpe: Type): Option[Type] = tpe.baseType(keyTagTpe.typeSymbol) match {
      case TypeRef(_, _, List(k, _)) => Some(k)
      case _ => None
    }

    private def isAbstract(tpe: Type): Boolean = tpe.exists {
      case TypeRef(_, sym, _) => sym.isType && !sym.isClass && (sym.isParameter || sym.isAbstract)
      case SingleType(_, sym) => sym.isParameter
      case _ => false
    }

    private def replacement(fields: List[Type], field: Type): Either[Refusal, Option[Int]] = {
      val requestedKey = key(field)
      // Shapeless's exact field extractor does not recognize refinements of a field. Read KeyTag's base type so
      // a narrower field still blocks an update of the same key.
      @annotation.tailrec def loop(rest: List[Type], index: Int): Either[Refusal, Option[Int]] = rest match {
        case head :: _ if head =:= field => Right(Some(index))
        case head :: tail =>
          (key(head), requestedKey) match {
            case (Some(hk), Some(fk)) if hk =:= fk => Left(ExistingKey(fk.toString))
            case (Some(hk), Some(fk)) if isAbstract(hk) || isAbstract(fk) => Left(Undecided(head.toString))
            case (None, Some(_)) if isAbstract(head) => Left(Undecided(head.toString))
            case (Some(_), None) if isAbstract(field) => Left(Undecided(head.toString))
            case _ => loop(tail, index + 1)
          }
        case Nil => Right(None)
      }
      loop(fields, 0)
    }

    def instImpl[T <: HList: WeakTypeTag, F: WeakTypeTag]: Tree = {
      val record = weakTypeOf[T].dealias
      val field = weakTypeOf[F].dealias
      val fields = unpackHListTpe(record)
      replacement(fields, field).fold(
        reason => c.abort(c.enclosingPosition, s"Cannot update record $record with field $field. ${reason.message}"),
        index => {
          val out = mkHListTpe(index.fold(fields :+ field)(fields.updated(_, field)))
          val position = index.getOrElse(fields.length)
          q"""
            new _root_.formless.record.Updater[$record, $field] {
              type Out = $out
              private val updater = new _root_.shapeless.ops.record.UnsafeUpdater($position)
              def apply(t: $record, f: $field): Out = updater(t, f).asInstanceOf[Out]
            }
          """
        },
      )
    }
  }
}
