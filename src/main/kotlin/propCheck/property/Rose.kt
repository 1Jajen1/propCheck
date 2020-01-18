package propCheck.property

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.fx
import arrow.core.extensions.sequence.foldable.foldLeft
import arrow.core.extensions.sequence.foldable.foldRight
import arrow.core.extensions.sequence.traverse.traverse
import arrow.extension
import arrow.mtl.typeclasses.ComposedFunctor
import arrow.mtl.typeclasses.Nested
import arrow.mtl.typeclasses.nest
import arrow.mtl.typeclasses.unnest
import arrow.recursion.typeclasses.Birecursive
import arrow.typeclasses.*
import propCheck.property.rosef.functor.functor

// @higherkind boilerplate
class ForRose private constructor() {
    companion object
}
typealias RoseOf<M, A> = arrow.Kind<RosePartialOf<M>, A>
typealias RosePartialOf<M> = arrow.Kind<ForRose, M>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <M, A> RoseOf<M, A>.fix(): Rose<M, A> =
    this as Rose<M, A>

data class Rose<M, A>(val runRose: Kind<M, RoseF<A, Rose<M, A>>>) :
    RoseOf<M, A> {

    fun <B> map(MF: Functor<M>, f: (A) -> B): Rose<M, B> = MF.run {
        Rose(runRose.map { RoseF(f(it.res), it.shrunk.map { it.map(MF, f) }) })
    }

    fun <B> ap(MA: Applicative<M>, ff: Rose<M, (A) -> B>): Rose<M, B> = MA.run {
        Rose(
            MA.map(runRose, ff.runRose) { (a, f) ->
                RoseF(
                    f.res(a.res),
                    f.shrunk
                        .map { it.map(MA) { it(a.res) } } +
                            a.shrunk
                                .map { it.ap(MA, ff) }

                )
            }
        )
    }


    fun <B> flatMap(MM: Monad<M>, f: (A) -> Rose<M, B>): Rose<M, B> =
        Rose(
            MM.fx.monad {
                val rose1 = !runRose
                val rose2 = !f(rose1.res).runRose
                RoseF(
                    rose2.res,
                    rose1.shrunk.map { it.flatMap(MM, f) } + rose2.shrunk
                )
            }
        )

    fun expand(MM: Monad<M>, f: (A) -> Sequence<A>): Rose<M, A> = MM.run {
        Rose(
            runRose.flatMap { r ->
                just(
                    RoseF(
                        r.res,
                        r.shrunk.map { it.expand(MM, f) } +
                                unfoldForest(MM, r.res, f)
                    )
                )
            }
        )
    }

    fun prune(MM: Monad<M>, n: Int): Rose<M, A> =
        if (n <= 0) Rose(
            MM.run { runRose.map { RoseF(it.res, emptySequence<Rose<M, A>>()) } }
        )
        else Rose(
            MM.run { runRose.map { RoseF(it.res, it.shrunk.map { it.prune(MM, n - 1) }) } }
        )

    companion object {
        fun <M, A> just(AM: Applicative<M>, a: A): Rose<M, A> =
            Rose(AM.just(RoseF(a, emptySequence())))

        fun <M, A> lift(FF: Functor<M>, fa: Kind<M, A>): Rose<M, A> = FF.run {
            Rose(fa.map { RoseF<A, Rose<M, A>>(it, emptySequence()) })
        }



        fun <M, A> unfold(MM: Monad<M>, a: A, f: (A) -> Sequence<A>): Rose<M, A> = Rose(
            MM.just(
                RoseF(a, unfoldForest(MM, a, f))
            )
        )

        fun <M, A> unfoldForest(MM: Monad<M>, a: A, f: (A) -> Sequence<A>): Sequence<Rose<M, A>> =
            f(a).map { unfold(MM, it, f) }
    }
}

class ForRoseF private constructor()
typealias RoseFOf<A, F> = arrow.Kind<RoseFPartialOf<A>, F>
typealias RoseFPartialOf<A> = arrow.Kind<ForRoseF, A>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <A, F> RoseFOf<A, F>.fix(): RoseF<A, F> =
    this as RoseF<A, F>

/**
 * Recursive data structure.
 * At every level keeps both the current tested value and (lazily) the shrunk values
 */
data class RoseF<A, F>(val res: A, val shrunk: Sequence<F>) : RoseFOf<A, F> {
    fun <B> map(f: (F) -> B): RoseF<A, B> = RoseF(res, shrunk.map(f))

    companion object
}

@extension
interface RoseFFunctor<C> : Functor<RoseFPartialOf<C>> {
    override fun <A, B> Kind<RoseFPartialOf<C>, A>.map(f: (A) -> B): Kind<RoseFPartialOf<C>, B> =
        fix().map(f)
}

@extension
interface RoseFTraverse<C> : Traverse<RoseFPartialOf<C>> {
    override fun <A, B> Kind<RoseFPartialOf<C>, A>.foldLeft(b: B, f: (B, A) -> B): B =
        fix().shrunk.foldLeft(b, f)

    override fun <A, B> Kind<RoseFPartialOf<C>, A>.foldRight(lb: Eval<B>, f: (A, Eval<B>) -> Eval<B>): Eval<B> =
        fix().shrunk.foldRight(lb, f)

    override fun <G, A, B> Kind<RoseFPartialOf<C>, A>.traverse(
        AP: Applicative<G>,
        f: (A) -> Kind<G, B>
    ): Kind<G, Kind<RoseFPartialOf<C>, B>> = AP.run {
        fix().shrunk.traverse(AP, f).map {
            RoseF(fix().res, it.fix())
        }
    }
}

@extension
interface RoseFunctor<M> : Functor<RosePartialOf<M>> {
    fun FM(): Functor<M>

    override fun <A, B> Kind<RosePartialOf<M>, A>.map(f: (A) -> B): Kind<RosePartialOf<M>, B> =
        fix().map(FM(), f)
}

@extension
interface RoseApplicative<M> : Applicative<RosePartialOf<M>> {
    fun MA(): Applicative<M>

    override fun <A> just(a: A): Kind<RosePartialOf<M>, A> =
        Rose.just(MA(), a)

    override fun <A, B> Kind<RosePartialOf<M>, A>.ap(ff: Kind<RosePartialOf<M>, (A) -> B>): Kind<RosePartialOf<M>, B> =
        fix().ap(MA(), ff.fix())

}

@extension
interface RoseMonad<M> : Monad<RosePartialOf<M>> {
    fun MM(): Monad<M>

    override fun <A, B> Kind<RosePartialOf<M>, A>.flatMap(f: (A) -> Kind<RosePartialOf<M>, B>): Kind<RosePartialOf<M>, B> =
        fix().flatMap(MM()) { f(it).fix() }

    override fun <A> just(a: A): Kind<RosePartialOf<M>, A> =
        Rose.just(MM(), a)

    override fun <A, B> tailRecM(a: A, f: (A) -> Kind<RosePartialOf<M>, Either<A, B>>): Kind<RosePartialOf<M>, B> =
        f(a).flatMap {
            it.fold({
                tailRecM(it, f)
            }, {
                just(it)
            })
        }
}

@extension
interface RoseAlternative<M> : Alternative<RosePartialOf<M>>, RoseApplicative<M> {
    fun AM(): Alternative<M>
    override fun MA(): Applicative<M> = AM()

    override fun <A> empty(): Kind<RosePartialOf<M>, A> = Rose.lift(AM(), AM().empty<A>())

    override fun <A> Kind<RosePartialOf<M>, A>.orElse(b: Kind<RosePartialOf<M>, A>): Kind<RosePartialOf<M>, A> =
        AM().run {
            Rose(fix().runRose.orElse(b.fix().runRose))
        }

    override fun <A> Kind<RosePartialOf<M>, A>.combineK(y: Kind<RosePartialOf<M>, A>): Kind<RosePartialOf<M>, A> =
        fix().orElse(y.fix())
}

@extension
interface RoseFunctorFilter<M> : FunctorFilter<RosePartialOf<M>>, RoseFunctor<M> {
    override fun FM(): Functor<M> = MM()
    fun MM(): Monad<M>
    fun AM(): Alternative<M>

    override fun <A, B> Kind<RosePartialOf<M>, A>.filterMap(f: (A) -> Option<B>): Kind<RosePartialOf<M>, B> =
        Rose(MM().fx.monad {
            val (x, xs) = fix().runRose.bind()
            f(x).fold({ AM().empty<RoseF<B, Rose<M, B>>>().bind() }, { x1 ->
                RoseF(x1, xs.map { it.filterMap(f).fix() })
            })
        })
}

@extension
interface RoseMonadFilter<M> : MonadFilter<RosePartialOf<M>>, RoseFunctorFilter<M>, RoseMonad<M> {
    override fun AM(): Alternative<M>
    override fun MM(): Monad<M>

    override fun <A> empty(): Kind<RosePartialOf<M>, A> = Rose.lift(MM(), AM().empty())
    override fun <A, B> Kind<RosePartialOf<M>, A>.map(f: (A) -> B): Kind<RosePartialOf<M>, B> =
        fix().map(MM(), f)

    override fun <A, B> Kind<RosePartialOf<M>, A>.filterMap(f: (A) -> Option<B>): Kind<RosePartialOf<M>, B> =
        Rose(MM().fx.monad {
            val (x, xs) = fix().runRose.bind()
            f(x).fold({ AM().empty<RoseF<B, Rose<M, B>>>().bind() }, { x1 ->
                RoseF(x1, xs.map { it.filterMap(f).fix() })
            })
        })
}

@extension
interface RoseBirecursive<M, A>: Birecursive<Rose<M, A>, Nested<M, RoseFPartialOf<A>>> {
    fun MM(): Monad<M>
    override fun FF(): Functor<Nested<M, RoseFPartialOf<A>>> =
        ComposedFunctor(MM(), RoseF.functor())

    override fun Kind<Nested<M, RoseFPartialOf<A>>, Rose<M, A>>.embedT(): Rose<M, A> =
        Rose(MM().run { unnest().map { it.fix() } })

    override fun Rose<M, A>.projectT(): Kind<Nested<M, RoseFPartialOf<A>>, Rose<M, A>> =
        runRose.nest()
}

fun <M, N, A> Rose<M, A>.hoist(f: FunctionK<M, N>, MF: Functor<M>): Rose<N, A> = Rose(
    MF.run {
        f(runRose.map { RoseF(it.res, it.shrunk.map { it.hoist(f, MF) }) })
    }
)

fun <A> Sequence<A>.splits(): Sequence<Tuple3<Sequence<A>, A, Sequence<A>>> =
    firstOrNull().toOption().fold({
        emptySequence()
    }, { x ->
        sequenceOf(Tuple3(emptySequence<A>(), x, drop(1)))
                // flatMap for added laziness
            .flatMap {
                sequenceOf(it) + drop(1).splits().map { (a, b, c) ->
                    Tuple3(sequenceOf(x) + a, b, c)
                }
            }
    })

fun <M, A> Sequence<RoseF<A, Rose<M, A>>>.dropOne(MM: Monad<M>): Sequence<Rose<M, Sequence<A>>> =
    SequenceK.fx {
        val (xs, _, zs) = !splits().k()
        Rose(MM.just((xs + zs).interleave(MM)))
    }

fun <M, A> Sequence<RoseF<A, Rose<M, A>>>.shrinkOne(MM: Monad<M>): Sequence<Rose<M, Sequence<A>>> =
    SequenceK.fx {
        val (xs, y, zs) = !splits().k()
        val y1 = !y.shrunk.k()
        Rose(
            MM.run {
                y1.runRose.map { (xs + sequenceOf(it) + zs).interleave(MM) }
            }
        )
    }

fun <M, A> Sequence<RoseF<A, Rose<M, A>>>.interleave(MM: Monad<M>): RoseF<Sequence<A>, Rose<M, Sequence<A>>> =
    RoseF(
        this.map { it.res },
        dropOne(MM) + shrinkOne(MM)
    )

