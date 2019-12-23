package propCheck.arbitrary

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.id.monad.monad
import arrow.core.extensions.list.monadFilter.filterMap
import arrow.extension
import arrow.mtl.OptionT
import arrow.mtl.OptionTPartialOf
import arrow.mtl.extensions.optiont.monad.monad
import arrow.mtl.value
import arrow.typeclasses.Functor
import arrow.typeclasses.Show
import propCheck.arbitrary.fn.functor.functor
import propCheck.arbitrary.fn.functor.map
import propCheck.arbitrary.gent.applicative.applicative
import propCheck.property.Rose
import propCheck.property.RoseF
import propCheck.property.rose.birecursive.birecursive

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
    operator fun component1(): (A) -> B = abstract(fn).f.andThen { opt ->
        opt.fold(
            { d },
            {
                it.runRose.value().value()
                    .fold({ throw IllegalStateException("Empty generator in function") }, { it.res })
            })
    }

    companion object
}

@extension
interface FunShow<A, B> : Show<Fun<A, B>> {
    fun SA(): Show<A>
    fun SB(): Show<B>

    override fun Fun<A, B>.show(): String = fn.table().let { ls ->
        if (ls.isEmpty()) "_ -> " + SB().run { d.show() }
        else ls.toList()
            .filterMap { (k, v) -> v.runRose.value().value().map { k toT it.res } }
            .joinToString("\n") { (k, v) ->
                SA().run { k.show() } + " -> " + SB().run { v.show() }
            } + "\n" + "_ -> " + SB().run { d.show() }
    }
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
        is Fn.MapFn<*, *, A> -> Fn.MapFn(
            t.f as (Any?) -> C,
            t.cF as (C) -> Any?,
            (t.g as Fn<C, A>).map(f).fix()
        ) as Fn<C, B>
    }
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

fun <A, B> abstract(fn: Fn<A, B>): Function1<A, Option<B>> = when (fn) {
    is Fn.UnitFn -> Function1 { fn.b.some() }
    is Fn.NilFn -> Function1 { None }
    is Fn.PairFn<*, *, B> -> Function1 { (x, y): Tuple2<Any?, Any?> ->
        Fn.functor<B>().run {
            abstract(fn.fn.map { (abstract(it).f as (Any?) -> B)(y) }).f(x)
        }
    } as Function1<A, Option<B>>
    is Fn.EitherFn<*, *, B> -> Function1 { e: Either<Any?, Any?> ->
        e.fold({
            (abstract(fn.l).f as (Any?) -> Option<B>)(it)
        }, {
            (abstract(fn.r).f as (Any?) -> Option<B>)(it)
        })
    } as Function1<A, Option<B>>
    is Fn.MapFn<*, *, B> -> Function1 { (abstract(fn.g).f as (Any?) -> Option<B>)((fn.f as (A) -> Any?)(it)) }
}

fun <A, B> Fn<A, B>.table(): Map<A, B> = when (this) {
    is Fn.UnitFn -> mapOf(Unit to b) as Map<A, B> // also safe, please add gadts
    is Fn.NilFn -> emptyMap()
    is Fn.EitherFn<*, *, B> -> (l.table().mapKeys { it.key.left() } + r.table().mapKeys { it.key.right() }) as Map<A, B>
    is Fn.PairFn<*, *, B> -> fn.table().toList().flatMap { (k, q) ->
        q.table().toList().map { (k2, v) -> (k toT k2) to v }
    }.toMap() as Map<A, B>
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
}

private fun <A, B, C> combineFn(l: Fn<A, C>, r: Fn<B, C>): Fn<Either<A, B>, C> =
    if (l is Fn.NilFn && r is Fn.NilFn) Fn.NilFn()
    else Fn.EitherFn(l, r)

interface Func<A> {
    fun <B> function(f: (A) -> B): Fn<A, B>
}

fun <A, B, C> funMap(fb: Func<B>, f: (A) -> B, cF: (B) -> A, g: (A) -> C): Fn<A, C> = fb.run {
    Fn.MapFn(f, cF, function { g(cF(it)) })
}

fun <A, B, C> funPair(fA: Func<A>, fB: Func<B>, f: (Tuple2<A, B>) -> C): Fn<Tuple2<A, B>, C> = fA.run {
    fB.run {
        Fn.PairFn(
            function(f.curry()).map { function(it) }
        )
    }
}

private fun <A, B, C> ((Tuple2<A, B>) -> C).curry(): ((A) -> ((B) -> C)) = { a -> { b -> this(a toT b) } }

fun <A, B, C> funEither(fA: Func<A>, fB: Func<B>, f: (Either<A, B>) -> C): Fn<Either<A, B>, C> =
    Fn.EitherFn(
        fA.run { function { f(it.left()) } },
        fB.run { function { f(it.right()) } }
    )

// instances
// Func
fun unitFunc(): Func<Unit> = object : Func<Unit> {
    override fun <B> function(f: (Unit) -> B): Fn<Unit, B> = Fn.UnitFn(f(Unit))
}

// Keep that here because I don't want to write other instances for Tuple3+ in the autogen file
@extension
interface Tuple2Func<A, B> : Func<Tuple2<A, B>> {
    fun AF(): Func<A>
    fun BF(): Func<B>

    override fun <C> function(f: (Tuple2<A, B>) -> C): Fn<Tuple2<A, B>, C> = funPair(AF(), BF(), f)
}

@extension
interface EitherFunc<L, R> : Func<Either<L, R>> {
    fun LF(): Func<L>
    fun RF(): Func<R>

    override fun <B> function(f: (Either<L, R>) -> B): Fn<Either<L, R>, B> = funEither(LF(), RF(), f)
}

interface BooleanFunc : Func<Boolean> {
    override fun <B> function(f: (Boolean) -> B): Fn<Boolean, B> = TODO()
}

// go straight to a list of single bit's encoded as boolean
interface LongFunc : Func<Long> {
    override fun <B> function(f: (Long) -> B): Fn<Long, B> = TODO()
}

