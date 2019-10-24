package propCheck.arbitrary

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.show
import arrow.extension
import arrow.typeclasses.Functor
import arrow.typeclasses.Show
import propCheck.arbitrary.`fun`.arbitrary.arbitrary
import propCheck.arbitrary.`fun`.show.show
import propCheck.arbitrary.fn.arbitrary.arbitrary
import propCheck.arbitrary.fn.functor.functor
import propCheck.arbitrary.fn.functor.map
import propCheck.arbitrary.gen.applicative.applicative
import propCheck.forAll
import propCheck.instances.arbitrary
import propCheck.instances.func
import propCheck.propCheck

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

class Fun<A, B>(val fn: Fn<A, B>, val d: B, val shrunk: Boolean, val f: (A) -> B) : FunOf<A, B> {
    operator fun component1(): (A) -> B = f

    companion object
}

@extension
interface FunFunctor<C> : Functor<FunPartialOf<C>> {
    override fun <A, B> Kind<FunPartialOf<C>, A>.map(f: (A) -> B): Kind<FunPartialOf<C>, B> = with(fix()) {
        Fun(Fn.functor<C>().run { fn.map(f).fix() }, f(d), shrunk, this.f.andThen(f))
    }
}

@extension
interface FunShow<A, B> : Show<Fun<A, B>> {
    fun SA(): Show<A>
    fun SB(): Show<B>

    override fun Fun<A, B>.show(): String = if (shrunk) showFn(fn, d, SA(), SB()) else "<fun>"
}

@extension
interface FunArbitrary<A, B> : Arbitrary<Fun<A, B>> {
    fun FA(): Func<A>
    fun AB(): Arbitrary<B>

    override fun arbitrary(): Gen<Fun<A, B>> =
        Gen.applicative().map(Fn.arbitrary(FA(), AB()).arbitrary(), AB().arbitrary()) { (fn, d) ->
            Fun(fn, d, false, abstract(fn, d).f)
        }.fix()

    override fun shrink(fail: Fun<A, B>): Sequence<Fun<A, B>> = Fn.arbitrary(FA(), AB()).shrink(fail.fn).map {
        Fun(it, fail.d, false, abstract(it, fail.d).f)
    } + (if (!fail.shrunk) sequenceOf(Fun(fail.fn, fail.d, true, fail.f)) else emptySequence())
}

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

    class TableFn<A, B>(val m: Map<A, Eval<B>>) : Fn<A, B>()

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
        is Fn.TableFn -> Fn.TableFn(t.m.mapValues { it.value.map(f) })
        is Fn.MapFn<*, *, A> -> Fn.MapFn(
            t.f as (Any?) -> C,
            t.cF as (C) -> Any?,
            (t.g as Fn<C, A>).map(f).fix()
        ) as Fn<C, B>
    }
}

@extension
interface FnArbitrary<A, B> : Arbitrary<Fn<A, B>> {
    fun FA(): Func<A>
    fun AB(): Arbitrary<B>

    override fun arbitrary(): Gen<Fn<A, B>> = FA().run {
        AB().arbitrary().map { b -> function { b } }
    }

    override fun shrink(fail: Fn<A, B>): Sequence<Fn<A, B>> = shrinkFun(fail) { AB().shrink(it) }
}

fun <A, B> showFn(fn: Fn<A, B>, d: B, sA: Show<A>, sB: Show<B>): String =
    (fn.table().entries.fold(emptyList<String>()) { acc, (k, v) ->
        sA.run {
            sB.run {
                acc + listOf(k.show() + " -> " + v.show())
            }
        }
    } + listOf("_ -> ${sB.run { d.show() }}")).toString()

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

private fun <A, B, C> ((Tuple2<A, B>) -> C).curry(): ((A) -> ((B) -> C)) =
    { a -> { b -> this(a toT b) } }

fun <A, B, C> funEither(fA: Func<A>, fB: Func<B>, f: (Either<A, B>) -> C): Fn<Either<A, B>, C> =
    Fn.EitherFn(
        fA.run { function { f(it.left()) } },
        fB.run { function { f(it.right()) } }
    )

fun unitFunc(): Func<Unit> = object : Func<Unit> {
    override fun <B> function(f: (Unit) -> B): Fn<Unit, B> = Fn.UnitFn(f(Unit))
}

inline fun <reified A : Enum<A>, B> funEnum(noinline f: (A) -> B): Fn<A, B> =
    funList(enumValues<A>().toList(), f)

fun <A, B> funList(vals: Collection<A>, f: (A) -> B): Fn<A, B> =
    Fn.TableFn(vals.map { it toT Eval.later { f(it) } }.toMap())


// Keep that here because I don't want to write other instances for Tuple3+ in the autogen file
@extension
interface Tuple2Func<A, B> : Func<Tuple2<A, B>> {
    fun AF(): Func<A>
    fun BF(): Func<B>

    override fun <C> function(f: (Tuple2<A, B>) -> C): Fn<Tuple2<A, B>, C> = funPair(AF(), BF(), f)
}
