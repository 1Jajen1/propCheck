package propCheck.arbitrary

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.id.monad.monad
import arrow.core.extensions.list.functorFilter.filterMap
import arrow.extension
import arrow.mtl.OptionT
import arrow.mtl.OptionTPartialOf
import arrow.mtl.extensions.optiont.applicative.applicative
import arrow.mtl.extensions.optiont.monad.monad
import arrow.mtl.value
import arrow.syntax.collections.tail
import arrow.typeclasses.Functor
import arrow.typeclasses.Monad
import arrow.typeclasses.Show
import propCheck.arbitrary.`fun`.show.show
import propCheck.arbitrary.either.func.func
import propCheck.arbitrary.fn.functor.functor
import propCheck.arbitrary.fn.functor.map
import propCheck.arbitrary.gent.applicative.applicative
import propCheck.arbitrary.listk.func.func
import propCheck.arbitrary.option.func.func
import propCheck.arbitrary.tuple2.func.func
import propCheck.property.PropertyT
import propCheck.property.Rose
import propCheck.property.fix
import propCheck.property.forAll
import propCheck.property.propertyt.monad.monad

// @higherkind boilerplate
class ForFun private constructor() {
    companion object
}
typealias FunOf<A, B> = arrow.Kind2<ForFun, A, B>
typealias FunPartialOf<A> = arrow.Kind<ForFun, A>
typealias FunKindedJ<A, B> = arrow.HkJ2<ForFun, A, B>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <A, B> FunOf<A, B>.fix(): Fun<A, B> =
    this as Fun<A, B>

class Fun<A, B>(val d: B, val fn: Fn<A, Rose<OptionTPartialOf<ForId>, B>>) : FunOf<A, B> {
    operator fun component1(): (A) -> B =
        abstract(fn, Rose.just(OptionT.applicative(Id.monad()), d)).map {
            it.runRose.value().value()
                .fold({ throw IllegalStateException("Empty generator in function") }, { it.res })
        }.f

    companion object
}

@extension
interface FunShow<A, B> : Show<Fun<A, B>> {
    fun SA(): Show<A>
    fun SB(): Show<B>

    // I might want to add a safeguard to this in terms of IO.timeout or something because when this renders badly shrunk,
    //  or unshrunk values, it will take ages!
    override fun Fun<A, B>.show(): String =
        fn.table().let { ls ->
            ls.toList()
                .filterMap { (k, v) -> v.runRose.value().value().map { k toT it.res } }
                .map { (k, v) ->
                    SA().run { k.show() } + " -> " + SB().run { v.show() }
                } + listOf("_ -> " + SB().run { d.show() })
        }.toString()
}

// Gen instance

// @higherkind boilerplate
class ForFn private constructor() {
    companion object
}
typealias FnOf<A, B> = arrow.Kind2<ForFn, A, B>
typealias FnPartialOf<A> = arrow.Kind<ForFn, A>
typealias FnKindedJ<A, B> = arrow.HkJ2<ForFn, A, B>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <A, B> FnOf<A, B>.fix(): Fn<A, B> =
    this as Fn<A, B>

sealed class Fn<A, B> : FnOf<A, B> {

    class UnitFn<B>(val b: B) : Fn<Unit, B>()

    class NilFn<A, B> : Fn<A, B>()

    class EitherFn<A, B, C>(val l: Fn<A, C>, val r: Fn<B, C>) : Fn<Either<A, B>, C>()

    class PairFn<A, B, C>(val fn: Fn<A, Fn<B, C>>) : Fn<Tuple2<A, B>, C>()

    class MapFn<A, B, C>(val f: (A) -> B, val cF: (B) -> A, val g: Fn<B, C>) : Fn<A, C>()

    class TableFn<A, B>(val m: Map<A, Eval<B>>) : Fn<A, B>()

    companion object
}

// Typesafety? No.
@extension
interface FnFunctor<C> : Functor<FnPartialOf<C>> {
    override fun <A, B> Kind<FnPartialOf<C>, A>.map(f: (A) -> B): Kind<FnPartialOf<C>, B> = when (val t = this.fix()) {
        is Fn.UnitFn -> Fn.UnitFn(f(t.b)) as Fn<C, B> // Safe because C == Unit, god I wish we had gadts here
        is Fn.NilFn -> Fn.NilFn()
        is Fn.EitherFn<*, *, A> -> Fn.EitherFn(
            (t.l as Fn<C, A>).map(f).fix(),
            (t.r as Fn<C, A>).map(f).fix()
        ) as Fn<C, B>
        is Fn.PairFn<*, *, A> -> Fn.PairFn((t.fn as Fn<C, Fn<C, A>>).map { it.map(f).fix() }.fix()) as Fn<C, B>
        is Fn.TableFn -> Fn.TableFn(t.m.mapValues { it.value.map(f) })
        is Fn.MapFn<*, *, A> -> Fn.MapFn(
            t.f as (Any?) -> C,
            t.cF as (C) -> Any?,
            (t.g as Fn<C, A>).map(f).fix()
        ) as Fn<C, B>
    }
}

// TODO move
fun <M, A, B> forAllFn(
    MM: Monad<M>,
    G: Gen<Fun<A, B>>,
    SA: Show<A> = Show.any(),
    SB: Show<B> = Show.any()
): PropertyT<M, (A) -> B> =
    PropertyT.monad(MM).run {
        forAll(G, MM, Fun.show(SA, SB)).map { (f) -> f }.fix()
    }

// fn gen
fun <A, B> Gen<B>.toFunction(AF: Func<A>, AC: Coarbitrary<A>): Gen<Fun<A, B>> =
    Gen.applicative(Id.monad()).map(
        this@toFunction,
        Gen { (s, sz) ->
            Rose.unfold(
                OptionT.monad(Id.monad()),
                AF.function { a -> AC.run { this@toFunction.coarbitrary(a) } }.map { it.runGen(s toT sz) }
            ) {
                shrinkFun(it) { it.runRose.value().value().fold({ emptySequence() }, { it.shrunk }) }
            }
        }
    ) { (d, fn) -> Fun(d, fn) }.fix()

fun <A, B> abstract(fn: Fn<A, B>, d: B): Function1<A, B> = when (fn) {
    is Fn.UnitFn -> Function1 { fn.b }
    is Fn.NilFn -> Function1 { d }
    is Fn.PairFn<*, *, B> -> Function1 { (x, y): Tuple2<Any?, Any?> ->
        Fn.functor<B>().run {
            abstract(fn.fn.map { (abstract(it, d).f as (Any?) -> B)(y) }, d).f(x)
        }
    } as Function1<A, B>
    is Fn.EitherFn<*, *, B> -> Function1 { e: Either<Any?, Any?> ->
        e.fold({
            (abstract(fn.l, d).f as (Any?) -> B)(it)
        }, {
            (abstract(fn.r, d).f as (Any?) -> B)(it)
        })
    } as Function1<A, B>
    is Fn.MapFn<*, *, B> -> Function1 { (abstract(fn.g, d).f as (Any?) -> B)((fn.f as (A) -> Any?)(it)) }
    is Fn.TableFn -> Function1 { fn.m.getOrDefault(it, Eval.now(d)).value() }
}

fun <A, B> Fn<A, B>.table(): Map<A, B> = when (this) {
    is Fn.UnitFn -> mapOf(Unit to b) as Map<A, B> // also safe, please add gadts
    is Fn.NilFn -> emptyMap()
    is Fn.EitherFn<*, *, B> -> (l.table().mapKeys { it.key.left() } + r.table().mapKeys { it.key.right() }) as Map<A, B>
    is Fn.PairFn<*, *, B> -> fn.table().toList().flatMap { (k, q) ->
        q.table().toList().map { (k2, v) -> (k toT k2) to v }
    }.toMap() as Map<A, B>
    is Fn.TableFn -> m.mapValues { it.value.value() }
    is Fn.MapFn<*, *, B> -> g.table().mapKeys { (cF as (Any?) -> A).invoke(it.key) }
}

fun <A, B> shrinkFun(fn: Fn<A, B>, shrinkB: (B) -> Sequence<B>): Sequence<Fn<A, B>> = when (fn) {
    is Fn.NilFn -> emptySequence()
    is Fn.UnitFn -> sequenceOf(Fn.NilFn<A, B>()) + (shrinkB(fn.b).map { Fn.UnitFn(it) } as Sequence<Fn<A, B>>)
    is Fn.PairFn<*, *, B> -> shrinkFun(fn.fn as Fn<Any?, Fn<Any?, B>>) { shrinkFun(it, shrinkB) }.map {
        when (it) {
            is Fn.NilFn -> Fn.NilFn()
            else -> Fn.PairFn(it) as Fn<A, B>
        }
    }
    is Fn.EitherFn<*, *, B> ->
        (sequenceOf(combineFn<Any?, Any?, B>(fn.l as Fn<Any?, B>, Fn.NilFn())) +
                sequenceOf(combineFn(Fn.NilFn(), fn.r as Fn<Any?, B>)) +
                shrinkFun(fn.l, shrinkB).map { combineFn(it, fn.r) } +
                shrinkFun(fn.r, shrinkB).map { combineFn(fn.l, it) }) as Sequence<Fn<A, B>>
    is Fn.MapFn<A, *, B> -> shrinkFun(fn.g, shrinkB).map {
        when (it) {
            is Fn.NilFn -> Fn.NilFn<A, B>()
            else -> Fn.MapFn(fn.f, fn.cF as (Any?) -> A, it as Fn<Any?, B>)
        }
    }
    is Fn.TableFn -> shrinkList(fn.m.toList()) { (a, evalB) ->
        sequenceOf(Unit).flatMap { evalB.map(shrinkB).value().map { a to Eval.now(it) } }
    }.map {
        if (it.isEmpty()) Fn.NilFn<A, B>()
        else Fn.TableFn(it.toMap())
    }
}

fun <A> shrinkList(list: List<A>, f: (A) -> Sequence<A>): Sequence<List<A>> {
    fun <F> removes(k: Int, n: Int, l: List<F>): Sequence<List<F>> =
        if (k > n) emptySequence()
        else if (l.isEmpty()) sequenceOf(emptyList())
        else sequenceOf(l.drop(k)) + sequenceOf(Unit).flatMap { removes(k, (n - k), l.drop(k)).map { l.take(k) + it } }

    fun shrinkListIt(l: List<A>): Sequence<List<A>> = when (l.size) {
        0 -> emptySequence()
        else -> iterate({ it / 2 }, l.size).takeWhile { it > 0 }
            .map { k -> removes(k, l.size, l) }.reduce { a, b -> a + b }
    }

    fun shrinkOne(l: List<A>): Sequence<List<A>> = when (l.size) {
        0 -> emptySequence()
        else -> f(l[0]).map { listOf(it) + l.drop(1) } +
                shrinkOne(l.drop(1)).map { listOf(l[0]) + it }
    }

    return if (list.isEmpty()) emptySequence()
    else shrinkListIt(list) + sequenceOf(Unit).flatMap { shrinkOne(list) }
}

fun <T : Any> iterate(f: (T) -> T, start: T) = generateSequence(start) { f(it) }

private fun <A, B, C> combineFn(l: Fn<A, C>, r: Fn<B, C>): Fn<Either<A, B>, C> =
    if (l is Fn.NilFn && r is Fn.NilFn) Fn.NilFn()
    else Fn.EitherFn(l, r)

interface Func<A> {
    fun <B> function(f: (A) -> B): Fn<A, B>
}

fun <A, B, C> funMap(fb: Func<B>, f: (A) -> B, cF: (B) -> A, g: (A) -> C): Fn<A, C> = fb.run {
    Fn.MapFn(f, cF, function(AndThen(cF).andThen(g)))
}

private fun <A, B, C> ((Tuple2<A, B>) -> C).curry(): ((A) -> ((B) -> C)) =
    { a -> { b -> this(a toT b) } }

fun <A, B, C> funPair(fA: Func<A>, fB: Func<B>, f: (Tuple2<A, B>) -> C): Fn<Tuple2<A, B>, C> = fA.run {
    fB.run {
        Fn.PairFn(
            function(f.curry()).map { function(it) }
        )
    }
}

fun <A, B, C> funEither(fA: Func<A>, fB: Func<B>, f: (Either<A, B>) -> C): Fn<Either<A, B>, C> =
    Fn.EitherFn(
        fA.run { function(AndThen(f).compose { it.left() }) },
        fB.run { function(AndThen(f).compose { it.right() }) }
    )

// instances
// Func
fun unitFunc(): Func<Unit> = object : Func<Unit> {
    override fun <B> function(f: (Unit) -> B): Fn<Unit, B> = Fn.UnitFn(f(Unit))
}

@extension
interface Tuple2Func<A, B> : Func<Tuple2<A, B>> {
    fun AF(): Func<A>
    fun BF(): Func<B>

    override fun <C> function(f: (Tuple2<A, B>) -> C): Fn<Tuple2<A, B>, C> =
        funPair(AF(), BF(), f)
}

@extension
interface EitherFunc<L, R> : Func<Either<L, R>> {
    fun LF(): Func<L>
    fun RF(): Func<R>

    override fun <B> function(f: (Either<L, R>) -> B): Fn<Either<L, R>, B> = funEither(LF(), RF(), f)
}

interface BooleanFunc : Func<Boolean> {
    override fun <B> function(f: (Boolean) -> B): Fn<Boolean, B> = funMap(Either.func(unitFunc(), unitFunc()), {
        if (it) Unit.right()
        else Unit.left()
    }, { it.isRight() }, f)
}

fun Boolean.Companion.func(): Func<Boolean> = object : BooleanFunc {}

// go straight to a list of single bit's encoded as boolean
interface LongFunc : Func<Long> {
    override fun <B> function(f: (Long) -> B): Fn<Long, B> =
        funMap(
            Tuple2.func(
                UByte.func(),
                Tuple2.func(
                    UByte.func(),
                    Tuple2.func(
                        UByte.func(),
                        Tuple2.func(
                            UByte.func(),
                            Tuple2.func(
                                UByte.func(),
                                Tuple2.func(UByte.func(), Tuple2.func(UByte.func(), UByte.func()))
                            )
                        )
                    )
                )
            ),
            {
                val l = it.toByteList().padTo(8, 0.toUByte())
                l[0] toT (l[1] toT (l[2] toT (l[3] toT (l[4] toT (l[5] toT (l[6] toT l[7]))))))
            },
            { (a, xs) ->
                listOf(a, xs.a, xs.b.a, xs.b.b.a, xs.b.b.b.a, xs.b.b.b.b.a, xs.b.b.b.b.b.a, xs.b.b.b.b.b.b).toLong()
            }, f
        )
}

fun Long.Companion.func(): Func<Long> = object : LongFunc {}

// TODO: do the other unsigned types
interface UByteFunc : Func<UByte> {
    override fun <B> function(f: (UByte) -> B): Fn<UByte, B> =
        funList((UByte.MIN_VALUE..UByte.MAX_VALUE).map { it.toUByte() }, f)
}

fun UByte.Companion.func(): Func<UByte> = object : UByteFunc {}

fun <A, B> funList(vals: Collection<A>, f: (A) -> B): Fn<A, B> =
    Fn.TableFn(vals.map { it toT Eval.later { f(it) } }.toMap())

private fun <A> List<A>.padTo(i: Int, a: A): List<A> =
    if (size < i) (this + listOf(a)).padTo(i, a)
    else this

private fun List<UByte>.toLong() = foldIndexed(0L) { i, acc, v ->
    acc or (v.toLong().shl(8 * i))
}

private fun Long.toByteList(): List<UByte> = when (this) {
    0L -> emptyList()
    else -> listOf(this.and(UByte.MAX_VALUE.toLong()).toUByte()) + this.ushr(8).toByteList()
}

@extension
interface OptionFunc<A> : Func<Option<A>> {
    fun AF(): Func<A>
    override fun <B> function(f: (Option<A>) -> B): Fn<Option<A>, B> =
        funMap(Either.func(unitFunc(), AF()), {
            it.toEither { Unit }
        }, {
            it.toOption()
        }, f)
}

@extension
interface ListKFunc<A> : Func<ListK<A>> {
    fun AF(): Func<A>
    override fun <B> function(f: (ListK<A>) -> B): Fn<ListK<A>, B> =
        funMap(Option.func(Tuple2.func(AF(), this)), {
            if (it.isEmpty()) None
            else (it.first() toT it.tail().k()).some()
        }, {
            it.fold({ ListK.empty() }, { (head, tail) ->
                (listOf(head) + tail).k()
            })
        }, f)
}

interface IntFunc : Func<Int> {
    override fun <B> function(f: (Int) -> B): Fn<Int, B> = funMap(Long.func(), {
        it.toLong()
    }, { it.toInt() }, f)
}

fun Int.Companion.func(): Func<Int> = object : IntFunc {}

interface ShortFunc : Func<Short> {
    override fun <B> function(f: (Short) -> B): Fn<Short, B> = funMap(Long.func(), {
        it.toLong()
    }, { it.toShort() }, f)
}

fun Short.Companion.func(): Func<Short> = object : ShortFunc {}

interface ByteFunc : Func<Byte> {
    override fun <B> function(f: (Byte) -> B): Fn<Byte, B> = funMap(Long.func(), {
        it.toLong()
    }, { it.toByte() }, f)
}

fun Byte.Companion.func(): Func<Byte> = object : ByteFunc {}

interface DoubleFunc : Func<Double> {
    override fun <B> function(f: (Double) -> B): Fn<Double, B> =
        funMap(Long.func(), { it.toRawBits() }, { Double.fromBits(it) }, f)
}

fun Double.Companion.func(): Func<Double> = object : DoubleFunc {}

interface FloatFunc : Func<Float> {
    override fun <B> function(f: (Float) -> B): Fn<Float, B> =
        funMap(Double.func(), { it.toDouble() }, { it.toFloat() }, f)
}

fun Float.Companion.func(): Func<Float> = object : FloatFunc {}

@extension
interface ConstFunc<A, T> : Func<Const<A, T>> {
    fun AF(): Func<A>
    override fun <B> function(f: (Const<A, T>) -> B): Fn<Const<A, T>, B> =
        funMap(AF(), { it.value() }, { Const(it) }, f)
}

@extension
interface IdFunc<A> : Func<Id<A>> {
    fun AF(): Func<A>
    override fun <B> function(f: (Id<A>) -> B): Fn<Id<A>, B> =
        funMap(AF(), { it.value() }, ::Id, f)
}

@extension
interface IorFunc<A, C> : Func<Ior<A, C>> {
    fun AF(): Func<A>
    fun BF(): Func<C>
    override fun <B> function(f: (Ior<A, C>) -> B): Fn<Ior<A, C>, B> =
        funMap(Either.func(Tuple2.func(AF(), BF()), Either.func(AF(), BF())), {
            it.fold({
                it.left().right()
            }, {
                it.right().right()
            }, { a, b ->
                (a toT b).left()
            })
        }, {
            it.fold({ (a, b) -> Ior.Both(a, b) }, {
                it.fold({ it.leftIor() }, { it.rightIor() })
            })
        }, f)
}

@extension
interface MapKFunc<K, V> : Func<MapK<K, V>> {
    fun KF(): Func<K>
    fun VF(): Func<V>
    override fun <B> function(f: (MapK<K, V>) -> B): Fn<MapK<K, V>, B> =
        funMap(ListK.func(Tuple2.func(KF(), VF())), {
            it.toList().map { it.toTuple2() }.k()
        }, {
            it.toMap().k()
        }, f)
}

@extension
interface SetVFunc<V> : Func<SetK<V>> {
    fun VF(): Func<V>
    override fun <B> function(f: (SetK<V>) -> B): Fn<SetK<V>, B> =
        funMap(ListK.func(VF()), {
            it.toList().k()
        }, {
            it.toSet().k()
        }, f)
}

@extension
interface NonEmptyListFunc<A> : Func<NonEmptyList<A>> {
    fun AF(): Func<A>
    override fun <B> function(f: (Nel<A>) -> B): Fn<Nel<A>, B> =
        funMap(Tuple2.func(AF(), ListK.func(AF())), {
            it.head toT it.tail.k()
        }, {
            Nel(it.a, it.b)
        }, f)
}

@extension
interface SequenceKFunc<A> : Func<SequenceK<A>> {
    fun AF(): Func<A>
    override fun <B> function(f: (SequenceK<A>) -> B): Fn<SequenceK<A>, B> =
        funMap(ListK.func(AF()), {
            it.toList().k()
        }, {
            it.asSequence().k()
        }, f)
}

interface SortedMapKFunc<K : Comparable<K>, V> : Func<SortedMapK<K, V>> {
    fun KF(): Func<K>
    fun VF(): Func<V>
    override fun <B> function(f: (SortedMapK<K, V>) -> B): Fn<SortedMapK<K, V>, B> =
        funMap(ListK.func(Tuple2.func(KF(), VF())), {
            it.toList().map { it.toTuple2() }.k()
        }, {
            it.toMap().toSortedMap().k()
        }, f)
}

fun <K : Comparable<K>, V> SortedMapK.Companion.func(KF: Func<K>, VF: Func<V>): Func<SortedMapK<K, V>> =
    object : SortedMapKFunc<K, V> {
        override fun KF(): Func<K> = KF
        override fun VF(): Func<V> = VF
    }

@extension
interface ValidatedFunc<E, A> : Func<Validated<E, A>> {
    fun EF(): Func<E>
    fun AF(): Func<A>
    override fun <B> function(f: (Validated<E, A>) -> B): Fn<Validated<E, A>, B> =
        funMap(Either.func(EF(), AF()), { it.toEither() }, {
            it.fold(::Invalid, ::Valid)
        }, f)
}

interface StringFunc : Func<String> {
    override fun <B> function(f: (String) -> B): Fn<String, B> =
        funMap(ListK.func(Char.func()), {
            it.toCharArray().toList().k()
        }, {
            it.joinToString("")
        }, f)
}

fun String.Companion.func(): Func<String> = object : StringFunc {}

interface CharFunc : Func<Char> {
    override fun <B> function(f: (Char) -> B): Fn<Char, B> =
        funMap(Long.func(), { it.toLong() }, { it.toChar() }, f)
}

fun Char.Companion.func(): Func<Char> = object : CharFunc {}
