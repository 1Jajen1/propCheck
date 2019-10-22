package propCheck.arbitrary

import arrow.Kind
import arrow.core.*
import arrow.extension
import arrow.typeclasses.Eq
import arrow.typeclasses.Functor
import arrow.typeclasses.Show

// @higherkind boilerplate
class ForFn private constructor() { companion object }
typealias FnOf<A, B> = arrow.Kind2<ForFn, A, B>
typealias FnPartialOf<A> = arrow.Kind<ForFn, A>
typealias FnKindedJ<A, B> = arrow.HkJ2<ForFn, A, B>
@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <A, B> FnOf<A, B>.fix(): Fn<A, B> =
    this as Fn<A, B>

sealed class Fn<A, B> : FnOf<A, B> {

    class UnitFn<B>(val b: B) : Fn<Unit, B>()

    class NilFn<A, B> : Fn<A, B>()

    class EitherFn<A, B, C>(val l: Fn<A, C>, val r: Fn<B, C>): Fn<Either<A, B>, C>()

    class PairFn<A, B, C>(val fn: Fn<A, Fn<B, C>>): Fn<Tuple2<A, B>, C>()

    class TableFn<A, B>(val m: Map<A, B>): Fn<A, B>()

    class MapFn<A, B, C>(val f: (A) -> B, val cF: (B) -> A, val g: Fn<B, C>): Fn<A, C>()

    companion object

}

// Typesafety? No.
@extension
interface FnFunctor<C> : Functor<FnPartialOf<C>> {
    override fun <A, B> Kind<FnPartialOf<C>, A>.map(f: (A) -> B): Kind<FnPartialOf<C>, B> = when (val t = this.fix()) {
        is Fn.UnitFn -> Fn.UnitFn(f(t.b)) as Fn<C, B> // Safe because C == Unit, god I wish we had gadts here
        is Fn.NilFn -> Fn.NilFn()
        is Fn.EitherFn<*, *, A> -> Fn.EitherFn((t.l as Fn<C, A>).map(f).fix(), (t.r as Fn<C, A>).map(f).fix()) as Fn<C, B>
        is Fn.PairFn<*, *, A> -> Fn.PairFn((t.fn as Fn<C, Fn<C, A>>).map { it.map(f).fix() }.fix()) as Fn<C, B>
        is Fn.TableFn -> Fn.TableFn(t.m.mapValues { f(it.value) })
        is Fn.MapFn<*, *, A> -> Fn.MapFn(t.f as (Any?) -> C, t.cF as (C) -> Any?, (t.g as Fn<C, A>).map(f).fix()) as Fn<C, B>
    }
}

@extension
interface ShowFn<A, B> : Show<Fn<A, B>> {
    fun SA(): Show<A>
    fun SB(): Show<B>

    override fun Fn<A, B>.show(): String = table().entries.fold(emptyList<String>()) { acc, (k, v) ->
        SA().run { SB().run {
            acc + listOf(k.show() + " -> " + v.show())
        } }
    }.toString()
}

fun <A, B> Fn<A, B>.table(): Map<A, B> = when (this) {
    is Fn.UnitFn -> mapOf(Unit to b) as Map<A, B> // also safe, please add gadts
    is Fn.NilFn -> emptyMap()
    is Fn.EitherFn<*, *, B> -> (l.table().mapKeys { it.key.left() } + r.table().mapKeys { it.key.right() }) as Map<A, B>
    is Fn.PairFn<*, *, B> -> fn.table().flatMap { it.value.table().asIterable() }.map { (it.key as A) toT it.value }.toMap()
    is Fn.TableFn -> m
    is Fn.MapFn<*, *, B> -> g.table().mapValues { (cF as (Any?) -> A).invoke(it.key) } as Map<A, B>
}


interface Func<A> {
    fun <B>function(f: (A) -> B): Fn<A, B>
}

fun <A, B, C>funMap(fb: Func<B>, f: (A) -> B, cF: (B) -> A, g: (A) -> C): Fn<A, C> = fb.run {
    Fn.MapFn(f, cF, function { g(cF(it)) })
}

fun <A, B, C>funPair(fA: Func<A>, fB: Func<B>, f: (Tuple2<A, B>) -> C): Fn<Tuple2<A, B>, C> = fA.run {
    fB.run {
        Fn.PairFn(
            function { a: A ->
                function {b: B ->
                    f(a toT b)
                }
            }
        )
    }
}

fun <A, B, C>funEither(fA: Func<A>, fB: Func<B>, f: (Either<A, B>) -> C): Fn<Either<A, B>, C> =
    Fn.EitherFn(
        fA.run { function { f(it.left()) } },
        fB.run { function { f(it.right()) } }
    )

fun unitFunc(): Func<Unit> = object: Func<Unit> {
    override fun <B> function(f: (Unit) -> B): Fn<Unit, B> = Fn.UnitFn(f(Unit))
}

inline fun <reified A: Enum<A>, B> funEnum(noinline f: (A) -> B): Fn<A, B> =
    funList(enumValues<A>().toList(), f)

fun <A, B> funList(vals: Collection<A>, f: (A) -> B): Fn<A, B> =
    Fn.TableFn(vals.map { it toT f(it) }.toMap())


// Keep that here because I don't want to write other instances for Tuple3+ in the autogen file
@extension
interface Tuple2Func<A, B> : Func<Tuple2<A, B>> {
    fun AF(): Func<A>
    fun BF(): Func<B>

    override fun <C> function(f: (Tuple2<A, B>) -> C): Fn<Tuple2<A, B>, C> = funPair(AF(), BF(), f)
}
