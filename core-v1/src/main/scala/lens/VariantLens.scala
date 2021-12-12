package com.rallyhealth.vapors.v1

package lens

import cats.arrow.Compose
import cats.data.NonEmptySet
import cats.kernel.Semigroup
import shapeless.ops.hlist
import shapeless.{Generic, HList}

import scala.collection.{Factory, MapView, View}

// TODO: Rename to NamedLens after old algebra removed
/**
  * @see [[VariantLens]] class for more details
  */
object VariantLens extends VariantLensLowPriorityImplicits {

  /**
    * The type of lens that returns the value it is given.
    */
  type Id[A] = VariantLens[A, A]

  /**
    * A singleton implementation of the [[identity]] lens.
    */
  val Id: VariantLens[Any, Any] = VariantLens(DataPath.empty, identity[Any])

  /**
    * The only definition of the [[Id]] lens.
    */
  def id[A]: VariantLens[A, A] = Id.asInstanceOf[Id[A]]

  /**
    * A function for building a lens from the [[Id]] lens.
    */
  type FromTo[A, +B] = VariantLens[A, A] => VariantLens[A, B]

  implicit object ComposeInstance extends Compose[VariantLens] {
    override def compose[A, B, C](
      f: VariantLens[B, C],
      g: VariantLens[A, B],
    ): VariantLens[A, C] =
      g.andThen(f)
  }

  /**
    * A fancy trick using macros that allows you to access the surrounding context of a method
    * invocation. By wrapping the call with this class, we can access the given [[lens]] to
    * apply the appropriate transformation.
    *
    * @param lens the lens to add the `.select` method to.
    */
  implicit final class Selector[A, B](val lens: VariantLens[A, B]) extends AnyVal {

    /**
      * Create a [[VariantLens]] that selects a field based on a given function and applies the appropriate
      * [[DataPath.atField]] operator.
      *
      * @note this uses a macro and only works for `val`s or `def`s with no arguments.
      *
      * @param getter a function that selects a value of type [[C]] from the return type [[B]] of the current lens
      */
    def select[C](getter: B => C): VariantLens[A, C] = macro VariantLensMacros.selectImpl[A, B, C]
  }

  /**
    * If the lens returns a [[IterableOnce]] of 2-tuples, then allow calling operations defined by [[AsMapBuilder]]
    */
  implicit def asMap[A, K, V](lens: VariantLens[A, IterableOnce[(K, V)]]): AsMapBuilder[A, K, V] =
    new AsMapBuilder[A, K, V](lens)

  final class AsMapBuilder[A, K, V](private val lens: VariantLens[A, IterableOnce[(K, V)]]) extends AnyVal {

    /**
      * Returns the right-side of all the 2-tuples as an [[Iterable]] of values.
      */
    def values: VariantLens[A, Iterable[V]] = lens.copy(
      get = lens.get.andThen(_.iterator.to(View).map(_._2)),
    )

    /**
      * @see [[AsIterableBuilder.to]] but the values are 2-tuples
      */
    def to[B](factory: Factory[(K, V), B]): VariantLens[A, B] =
      lens.copy(get = lens.get.andThen(factory.fromSpecific(_)))

    /**
      * Group the 2-tuples by the left side and create a [[Map]].
      *
      * If there are duplicate keys, then the last entry in the sequence wins the spot and the others are dropped.
      */
    def toMap: VariantLens[A, Map[K, V]] = lens.copy(
      get = lens.get.andThen(Map.from(_)),
    )

    /**
      * Same as [[toMap]], but creates a lazy [[MapView]] instead.
      */
    def toMapView: VariantLens[A, MapView[K, V]] = lens.copy(
      get = lens.get.andThen(Map.from(_).view),
    )
  }

  // TODO: Make this more generally useful and convert back to a NamedLens implicitly
  // I think this can be folded into VariantLens now that it utilizes variance
  final class AsIterableBuilder[A, C[x] <: IterableOnce[x], E](private val lens: VariantLens[A, C[E]]) extends AnyVal {

    /**
      * Use the given Scala collection companion object to convert this [[IterableOnce]] into the desired output.
      *
      * TODO: Should there be any path information for conversion?
      *       List => Map seems important information as values with duplicate keys could be dropped
      */
    def to[B](factory: Factory[E, B]): VariantLens[A, B] =
      lens.copy(get = lens.get.andThen(factory.fromSpecific(_)))

    /**
      * Get the first element of this [[IterableOnce]], if it isn't empty.
      */
    def headOption: VariantLens[A, Option[E]] =
      lens.copy(
        path = lens.path.atHead,
        get = lens.get.andThen(b => View.from[E](b).headOption),
      )
  }

  implicit final class AsHListBuilder[A, L <: HList](private val lens: VariantLens[A, L]) extends AnyVal {

    /**
      * Convert the resulting [[HList]] into a tuple.
      */
    def tupled[T](implicit tupler: hlist.Tupler.Aux[L, T]): VariantLens[A, T] =
      lens.copy(
        path = lens.path,
        get = lens.get.andThen(tupler.apply),
      )
  }
}

sealed trait VariantLensLowPriorityImplicits {

  /**
    * Wrap the result of this lens with an [[AsIterableBuilder]] for helper operations on [[IterableOnce]] types.
    */
  implicit def asIterable[A, B, C[x] <: IterableOnce[x], E](
    lens: VariantLens[A, B],
  )(implicit
    ev: B <:< C[E],
  ): VariantLens.AsIterableBuilder[A, C, E] =
    new VariantLens.AsIterableBuilder(lens.as[C[E]])
}

/**
  * A serializable lens for extracting a value from an object.
  *
  * @param path the serializable path to the value from the starting type to the value type
  * @param get a function for extracting the field value
  * @tparam A the starting type
  * @tparam B the extracted value type
  */
final case class VariantLens[-A, +B](
  path: DataPath,
  get: A => B,
) { outer =>

  /**
    * Composes another lens to run on the output of this lens.
    *
    * @note Because extension methods don't retain type information well,
    *       this is still useful despite the [[Compose]] instance.
    */
  def andThen[C](lens: VariantLens[B, C]): VariantLens[A, C] = {
    copy(
      path = this.path ++ lens.path,
      get = get.andThen(lens.get),
    )
  }

  /**
    * Access a field from the object returned by this lens.
    *
    * Typically, you would not call this directly, but instead by calling the [[VariantLens.Selector.select]]
    * extension method on this lens. The `.select` method uses macros to make sure that the `name` param is
    * set properly.
    *
    * @param name the name of the field
    * @param getter a function to extract the value of this field from the object
    * TODO: Use HList to make this more safe, instead of relying on the macro to use this properly.
    */
  def field[C](
    name: String,
    getter: B => C,
  ): VariantLens[A, C] = {
    copy(
      path = path.atField(name),
      get = this.get.andThen(getter),
    )
  }

  /**
    * Used for accessing the value for a given key in this object.
    *
    * This requires that there is an [[Indexed]] definition for how to access a value from the
    * indexable object returned by the current lens.
    *
    * @param key either a value of the appropriate key type OR a [[shapeless.Nat]] for accessing a
    *            specifically typed value from a tuple or [[HList]]
    */
  def at[K : ValidDataPathKey, V](
    key: K,
  )(implicit
    CI: VariantIndexed[B, K, V],
  ): VariantLens[A, V] = {
    copy(
      path = path.atKey(key),
      get = get.andThen(b => CI.get(b)(key)),
    )
  }

  /**
    * Filters the given set of keys from this indexable object.
    *
    * @note Unfortunately this does not work for [[HList]] the same way that [[at]] does.
    *
    * TODO: It may be possible to make this work for tuples / HList with varargs instead of [[NonEmptySet]].
    */
  def filterKeys[K : ValidDataPathKey, V : Semigroup](
    keys: NonEmptySet[K],
  )(implicit
    CI: VariantIndexed[B, K, V],
  ): VariantLens[A, V] = {
    import VariantIndexedSyntax._
    copy(
      path = path.filterKeys(keys),
      get = get.andThen(_.filterKeys(keys)),
    )
  }

  /**
    * Upcasts the lens.
    *
    * @note this would not be necessary if [[VariantLens]] used appropriate variance, however, variance for lenses
    *       introduces a lot of issues.
    */
  def as[V](implicit ev: B <:< V): VariantLens[A, V] =
    this.copy(get = this.get.andThen(ev))

  /**
    * Converts the result into an [[HList]] if possible.
    *
    * Uses recursive implicit resolution at compile-time to derive the appropriate heterogenious return type.
    *
    * @see [[Generic]]
    */
  def asHList[C >: B](implicit gen: Generic[C]): VariantLens[A, gen.Repr] = copy(get = this.get.andThen(gen.to(_)))
}
