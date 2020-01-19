package propCheck.arbitrary

import arrow.Kind
import arrow.core.*
import arrow.extension
import arrow.syntax.collections.tail
import arrow.typeclasses.Contravariant
import arrow.typeclasses.Decidable
import arrow.typeclasses.Divide
import arrow.typeclasses.Divisible
import propCheck.arbitrary.listk.coarbitrary.coarbitrary
import propCheck.arbitrary.tuple2.coarbitrary.coarbitrary

// @higherkind boilerplate
class ForCoarbitrary private constructor() {
    companion object
}
typealias CoarbitraryOf<A> = arrow.Kind<ForCoarbitrary, A>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <A> CoarbitraryOf<A>.fix(): Coarbitrary<A> =
    this as Coarbitrary<A>

interface Coarbitrary<A> : CoarbitraryOf<A> {

    fun <M, B> GenT<M, B>.coarbitrary(a: A): GenT<M, B>

    companion object
}

fun <M, B> GenT<M, B>.variant(i: Long): GenT<M, B> =
    GenT { (rand, size) ->
        runGen(rand.variant(i) toT size)
    }

// keep it here for now, maybe add that for TupleN later
@extension
interface Tuple2Coarbitrary<A, B> : Coarbitrary<Tuple2<A, B>> {
    fun CA(): Coarbitrary<A>
    fun CB(): Coarbitrary<B>
    override fun <M, C> GenT<M, C>.coarbitrary(a: Tuple2<A, B>): GenT<M, C> =
        CA().run {
            CB().run {
                coarbitrary(a.a).coarbitrary(a.b)
            }
        }
}

fun unitCoarbitrary(): Coarbitrary<Unit> = object : Coarbitrary<Unit> {
    override fun <M, B> GenT<M, B>.coarbitrary(a: Unit): GenT<M, B> = this
}

interface LongCoarbitrary : Coarbitrary<Long> {
    override fun <M, B> GenT<M, B>.coarbitrary(a: Long): GenT<M, B> = variant(a)
}

fun Long.Companion.coarbitrary(): Coarbitrary<Long> = object: LongCoarbitrary {}

interface IntCoarbitrary : Coarbitrary<Int> {
    override fun <M, B> GenT<M, B>.coarbitrary(a: Int): GenT<M, B> = variant(a.toLong())
}

fun Int.Companion.coarbitrary(): Coarbitrary<Int> = object: IntCoarbitrary {}

interface ShortCoarbitrary : Coarbitrary<Short> {
    override fun <M, B> GenT<M, B>.coarbitrary(a: Short): GenT<M, B> = variant(a.toLong())
}

fun Short.Companion.coarbitrary(): Coarbitrary<Short> = object: ShortCoarbitrary {}

interface ByteCoarbitrary : Coarbitrary<Byte> {
    override fun <M, B> GenT<M, B>.coarbitrary(a: Byte): GenT<M, B> = variant(a.toLong())
}

fun Byte.Companion.coarbitrary(): Coarbitrary<Byte> = object: ByteCoarbitrary {}

interface FloatCoarbitrary : Coarbitrary<Float> {
    override fun <M, B> GenT<M, B>.coarbitrary(a: Float): GenT<M, B> = variant(a.toDouble().toRawBits())
}

fun Float.Companion.coarbitrary(): Coarbitrary<Float> = object: FloatCoarbitrary {}

interface DoubleCoarbitrary : Coarbitrary<Double> {
    override fun <M, B> GenT<M, B>.coarbitrary(a: Double): GenT<M, B> = variant(a.toRawBits())
}

fun Double.Companion.coarbitrary(): Coarbitrary<Double> = object: DoubleCoarbitrary {}

interface CharCoarbitrary : Coarbitrary<Char> {
    override fun <M, B> GenT<M, B>.coarbitrary(a: Char): GenT<M, B> = variant(a.toLong())
}

fun Char.Companion.coarbitrary(): Coarbitrary<Char> = object: CharCoarbitrary {}

interface StringCoarbitrary : Coarbitrary<String> {
    override fun <M, B> GenT<M, B>.coarbitrary(a: String): GenT<M, B> =
        ListK.coarbitrary(Char.coarbitrary()).run {
            coarbitrary(a.toList().k())
        }
}

fun String.Companion.coarbitrary(): Coarbitrary<String> = object: StringCoarbitrary {}

@extension
interface ListKCoarbitrary<A> : Coarbitrary<ListK<A>> {
    fun AC(): Coarbitrary<A>
    override fun <M, B> GenT<M, B>.coarbitrary(a: ListK<A>): GenT<M, B> =
        a.foldLeft(variant(1)) { acc, v ->
            AC().run {
                acc.variant(2).coarbitrary(v)
            }
        }
}

@extension
interface NonEmptyListCoarbitrary<A> : Coarbitrary<NonEmptyList<A>> {
    fun AC(): Coarbitrary<A>
    override fun <M, B> GenT<M, B>.coarbitrary(a: Nel<A>): GenT<M, B> =
        ListK.coarbitrary(AC()).run {
            coarbitrary(a.all.k())
        }
}

@extension
interface OptionCoarbitrary<A> : Coarbitrary<Option<A>> {
    fun AC(): Coarbitrary<A>
    override fun <M, B> GenT<M, B>.coarbitrary(a: Option<A>): GenT<M, B> =
        a.fold({ variant(1) }, {
            AC().run {
                variant(2L).coarbitrary(it)
            }
        })
}

@extension
interface EitherCoarbitrary<L, R> : Coarbitrary<Either<L, R>> {
    fun LC(): Coarbitrary<L>
    fun RC(): Coarbitrary<R>
    override fun <M, B> GenT<M, B>.coarbitrary(a: Either<L, R>): GenT<M, B> =
        a.fold({
            LC().run { variant(1).coarbitrary(it) }
        }, {
            RC().run { variant(2).coarbitrary(it) }
        })
}

@extension
interface ConstCoarbitrary<A, T> : Coarbitrary<Const<A, T>> {
    fun AC(): Coarbitrary<A>
    override fun <M, B> GenT<M, B>.coarbitrary(a: Const<A, T>): GenT<M, B> =
        AC().run { coarbitrary(a.value()) }
}

@extension
interface IdCoarbitrary<A> : Coarbitrary<Id<A>> {
    fun AC(): Coarbitrary<A>
    override fun <M, B> GenT<M, B>.coarbitrary(a: Id<A>): GenT<M, B> =
        AC().run { coarbitrary(a.value()) }
}

@extension
interface IorCoarbitrary<L, R> : Coarbitrary<Ior<L, R>> {
    fun LC(): Coarbitrary<L>
    fun RC(): Coarbitrary<R>
    override fun <M, B> GenT<M, B>.coarbitrary(a: Ior<L, R>): GenT<M, B> =
        a.fold({
            LC().run { variant(1).coarbitrary(it) }
        }, {
            RC().run { variant(2).coarbitrary(it) }
        }, { l, r ->
            LC().run {
                RC().run {
                    variant(3).coarbitrary(l).coarbitrary(r)
                }
            }
        })
}

@extension
interface MapKCoarbitrary<K, V> : Coarbitrary<MapK<K, V>> {
    fun KC(): Coarbitrary<K>
    fun VC(): Coarbitrary<V>
    override fun <M, B> GenT<M, B>.coarbitrary(a: MapK<K, V>): GenT<M, B> =
        ListK.coarbitrary(Tuple2.coarbitrary(KC(), VC())).run {
            coarbitrary(a.toList().k().map { it.toTuple2() })
        }
}

@extension
interface SetKCoarbitrary<V> : Coarbitrary<SetK<V>> {
    fun VC(): Coarbitrary<V>
    override fun <M, B> GenT<M, B>.coarbitrary(a: SetK<V>): GenT<M, B> =
        ListK.coarbitrary(VC()).run {
            coarbitrary(a.toList().k())
        }
}

@extension
interface SequenceKCoarbitrary<A> : Coarbitrary<SequenceK<A>> {
    fun AC(): Coarbitrary<A>
    override fun <M, B> GenT<M, B>.coarbitrary(a: SequenceK<A>): GenT<M, B> =
        ListK.coarbitrary(AC()).run {
            coarbitrary(a.toList().k())
        }
}

interface SortedMapKCoarbitrary<K: Comparable<K>, V> : Coarbitrary<SortedMapK<K, V>> {
    fun KC(): Coarbitrary<K>
    fun VC(): Coarbitrary<V>
    override fun <M, B> GenT<M, B>.coarbitrary(a: SortedMapK<K, V>): GenT<M, B> =
        ListK.coarbitrary(Tuple2.coarbitrary(KC(), VC())).run {
            coarbitrary(a.toList().k().map { it.toTuple2() })
        }
}

fun <K: Comparable<K>, V> SortedMapK.Companion.coarbitrary(KC: Coarbitrary<K>, VC: Coarbitrary<V>) =
    object: SortedMapKCoarbitrary<K, V> {
        override fun KC(): Coarbitrary<K> = KC
        override fun VC(): Coarbitrary<V> = VC
    }

@extension
interface VaidatedCoarbitrary<E, A> : Coarbitrary<Validated<E, A>> {
    fun EC(): Coarbitrary<E>
    fun AC(): Coarbitrary<A>
    override fun <M, B> GenT<M, B>.coarbitrary(a: Validated<E, A>): GenT<M, B> =
        a.fold({
            EC().run {
                variant(1).coarbitrary(it)
            }
        }, {
            AC().run {
                variant(2).coarbitrary(it)
            }
        })
}
