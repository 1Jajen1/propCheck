package propCheck.property

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.sequence.foldable.foldLeft
import arrow.core.extensions.sequence.foldable.foldRight
import arrow.core.extensions.sequence.traverse.traverse
import arrow.extension
import arrow.fx.ForIO
import arrow.fx.IO
import arrow.fx.extensions.fx
import arrow.fx.extensions.io.applicativeError.handleError
import arrow.fx.extensions.io.monad.monad
import arrow.fx.fix
import arrow.typeclasses.*
import pretty.text

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

    fun <B> map(FM: Functor<M>, f: (A) -> B): Rose<M, B> = FM.run {
        Rose(runRose.map { RoseF(f(it.res), it.shrunk.map { it.map(FM, f) }) })
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

    companion object {
        fun <M, A> just(AM: Applicative<M>, a: A): Rose<M, A> =
            Rose(AM.just(RoseF(a, emptySequence())))
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
    fun MM(): Monad<M>

    override fun <A> just(a: A): Kind<RosePartialOf<M>, A> =
        Rose.just(MM(), a)

    override fun <A, B> Kind<RosePartialOf<M>, A>.ap(ff: Kind<RosePartialOf<M>, (A) -> B>): Kind<RosePartialOf<M>, B> =
        fix().flatMap(MM()) { a -> ff.fix().map(MM()) { f -> f(a) } }
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

/**
 * wrap an IO with results safely
 */
fun ioRose(rose: IO<Rose<ForIO, TestResult>>): Rose<ForIO, TestResult> =
    Rose(
        protectRose(rose).flatMap { it.runRose }
    )

/**
 * catch exceptions in io code and fail the test if necessary
 */
fun protectRose(rose: IO<Rose<ForIO, TestResult>>): IO<Rose<ForIO, TestResult>> = rose.handleError {
    Rose.just(
        IO.monad(),
        failed(
            reason = "Exception".text(),
            exception = it.some()
        )
    )
}

/**
 * apply a function on a roses content
 */
fun <A> onRose(rose: Rose<ForIO, A>, f: (A, Sequence<Rose<ForIO, A>>) -> Rose<ForIO, A>): Rose<ForIO, A> =
    Rose(
        IO.fx {
            val (res, shrunk) = !rose.runRose
            !f(res, shrunk).runRose
        }.fix()
    )

/**
 * catch exceptions in io code and fail if necessary
 */
fun protectResult(io: IO<TestResult>): IO<TestResult> = io.handleError {
    failed(
        reason = "Exception".text(),
        exception = it.some()
    )
}

/**
 * wrap internal roses with ioRose and install error handlers
 */
fun protectResults(rose: Rose<ForIO, TestResult>): Rose<ForIO, TestResult> =
    onRose(rose) { x, rs ->
        Rose(
            IO.fx {
                val y = protectResult(IO.just(x)).bind()
                RoseF(y, rs.map(::protectResults))
            }
        )
    }
