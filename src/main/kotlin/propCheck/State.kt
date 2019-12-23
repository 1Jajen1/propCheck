package propCheck

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.id.monad.monad
import arrow.core.extensions.list.foldable.foldLeft
import arrow.core.extensions.list.foldable.foldRight
import arrow.fx.typeclasses.Concurrent
import arrow.typeclasses.Monad
import propCheck.arbitrary.Gen
import propCheck.arbitrary.GenT
import propCheck.arbitrary.fix
import propCheck.arbitrary.monadGen
import propCheck.property.*
import propCheck.property.propertyt.applicative.applicative
import propCheck.property.propertyt.monad.monad
import propCheck.property.propertyt.monadTest.failure
import propCheck.property.propertyt.monadTest.succeeded
import java.util.concurrent.Executors
import kotlin.math.max

/**
 * State machine testing rework:
 * Keep current api, it's decent
 * Turn PROP into PropertyT<M, Unit> -- !
 * Parameterize by M -- !
 * Rework gen to hold invariants but also shrink ?? most difficult part here
 * Checking results needs to be rewritten to return PropertyT<M, Unit> -- !
 * Same for results in parallel
 * Syntax sugar for building PropertyM where M is fixed to IO or maybe just fix it all to IO?
 */

typealias PreCondition<S, A> = (S, A) -> Boolean

typealias PostCondition<S, A, R, M> = (S, A, R) -> PropertyT<M, Unit>
typealias Invariant<S> = (S) -> Boolean
typealias Transition<S, A> = (S, A) -> S

data class StateMachine<M, S, A, R, SUT>(
    val initialState: S,
    val cmdGen: (S) -> Gen<Option<A>>,
    val preCondition: PreCondition<S, A>,
    val invariant: Invariant<S>,
    val transition: Transition<S, A>,
    val executeAction: (A, SUT) -> Kind<M, R>,
    val sut: () -> Kind<M, SUT>
)

internal fun <M, S, A, R, SUT> commandGen(
    sm: StateMachine<M, S, A, R, SUT>,
    currState: S
): Gen<List<A>> = GenT.monadGen(Id.monad()).run {
    sized { sz ->
        if (sz == 0) GenT.just(Id.monad(), emptyList())
        else fx.monad {
            val (optCommands) = sm.cmdGen(currState).fromGenT().filter {
                it.fold({ true }, { sm.preCondition(currState, it) && sm.invariant(sm.transition(currState, it)) })
            }
            optCommands.fold({ emptyList<A>() }, {
                listOf(it) + commandGen(sm, sm.transition(currState, it)).fromGenT().resize(sz - 1).bind()
            })
        }
    }.fix()
}

internal fun <M, A, R, SUT> executeActions(
    actions: List<A>,
    sut: SUT,
    MM: Monad<M>,
    executeAction: (A, SUT) -> Kind<M, R>
): Kind<M, List<Tuple2<A, R>>> =
    actions.foldLeft(MM.just(emptyList())) { accIO, v ->
        MM.run {
            accIO.flatMap {
                executeAction(v, sut).map { r ->
                    it + listOf(Tuple2(v, r))
                }
            }
        }
    }

// TODO rewrite inner fold using StateT?
internal fun <M, S, A, R> checkResults(
    res: List<Tuple2<A, R>>,
    state: S,
    postCondition: PostCondition<S, A, R, M>,
    invariant: Invariant<S>,
    transition: Transition<S, A>,
    MM: Monad<M>
): Tuple2<PropertyT<M, Unit>, S> =
    res.foldLeft(
        PropertyT.monad(MM).just(Unit).fix() toT state
    ) { (prop, s), (a, r) ->
        PropertyT.monad(MM).fx.monad {
            prop.bind()
            // check invariant
            if (invariant(state)) succeeded(MM).bind()
            // check post condition
            postCondition(s, a, r).bind()
        }.fix() toT transition(s, a)
    }

fun <M, S, A, R, SUT> execSeq(
    sm: StateMachine<M, S, A, R, SUT>,
    MM: Monad<M>,
    postCondition: PostCondition<S, A, R, M>
): PropertyT<M, Unit> = PropertyT.propertyTestM(MM).fx.monad {
    val (cmds) = forAll(commandGen(sm, sm.initialState), MM)

    val (sut) = PropertyT.lift(MM, sm.sut())

    val (results) = PropertyT.lift(MM, executeActions(cmds, sut, MM, sm.executeAction))

    checkResults(results, sm.initialState, postCondition, sm.invariant, sm.transition, MM).a.bind()
}.fix()

// ------------ parallel
fun <M, S, A, R, SUT> parGen(sm: StateMachine<M, S, A, R, SUT>, maxThreads: Int) =
    commandGen(sm, sm.initialState).let { prefixGen ->
        GenT.monadGen(Id.monad()).run {
            fx.monad {
                val (prefix) = prefixGen.fromGenT()
                val s = prefix.foldLeft(sm.initialState) { s, a -> sm.transition(s, a) }
                val pathGen = commandGen(sm, s)
                val (threads) = int(2..maxThreads)
                Tuple2(
                    prefix,
                    (0 until threads).map {
                        pathGen.resize( // TODO short hand for sized(::identity)
                            int(0..sized { Gen.just(Id.monad(), it) }.bind()).bind() / threads
                        ).bind()
                    }
                )
            }
        }
    }.fix()

private val pool = Executors.newCachedThreadPool { r ->
    Executors.defaultThreadFactory().newThread(r).apply {
        isDaemon = true
    }
}

fun <M, S, A, R, SUT> execPar(
    sm: StateMachine<M, S, A, R, SUT>,
    maxThreads: Int = 2,
    MM: Concurrent<M>,
    postCondition: PostCondition<S, A, R, M>
): PropertyT<M, Unit> = PropertyT.propertyTestM(MM).fx.monad {
    val (prefix, paths) = forAll(parGen(sm, max(maxThreads, 2)), MM).bind()

    val (sut) = PropertyT.lift(MM, sm.sut())
    val (prefixResult) = PropertyT.lift(MM, executeActions(prefix, sut, MM, sm.executeAction))

    val (prefixRes, state) = checkResults(
        prefixResult, sm.initialState, postCondition,
        sm.invariant, sm.transition, MM
    )

    // check prefix
    prefixRes.bind()

    // check parallel actions
    // TODO check if this is actually parallel for IO, for other Concurrent instances idc, their responsibility
    val (pathResults) = PropertyT.lift(
        MM, MM.run {
            paths.parTraverse {
                executeActions(it, sut, MM, sm.executeAction).map { it.asSequence() }
            }
        }
    )

    // TODO annotate failures properly, probably needs to be done inside recurGo
    recurGo(pathResults, state, sm.invariant, sm.transition, postCondition, MM).bind()
}.fix()

// TODO where is the recursion scheme?!
internal fun <M, S, A, R> recurGo(
    list: List<Sequence<Tuple2<A, R>>>,
    state: S,
    invariant: Invariant<S>,
    transition: Transition<S, A>,
    postCondition: PostCondition<S, A, R, M>,
    MM: Monad<M>
): PropertyT<M, Unit> = list.filter { it.firstOrNull() != null }.let {
    // TODO How lazy is this? // test if stuff gets needlessly evaluated by checking if failure().flatMap(throw) throws
    PropertyT.monad(MM).fx.monad {
        if (it.isEmpty()) succeeded(MM).bind()
        else it.mapIndexed { i, _ -> go(it, state, i, invariant, transition, postCondition, MM) }
            .foldRight(Eval.now(failure(MM).fix())) { v, acc ->
                acc.map {
                    PropertyT.applicative(MM).tupled(v, it).map { Unit }.fix()
                }
            }.value().bind()
    }.fix()
}

internal fun <S, A, R, M> go(
    list: List<Sequence<Tuple2<A, R>>>,
    state: S,
    pos: Int,
    invariant: Invariant<S>,
    transition: Transition<S, A>,
    postCondition: PostCondition<S, A, R, M>,
    MM: Monad<M>
): PropertyT<M, Unit> = when {
    list[pos].firstOrNull() == null -> succeeded(MM)
    else -> {
        val (a, r) = list[pos].first()
        val newState = transition(state, a)
        PropertyT.monad(MM).fx.monad {
            // TODO annotate failure with better messages as to what exactly failed
            if (invariant(newState)) failure(MM).bind()
            postCondition(state, a, r).bind()
            recurGo(
                list.mapIndexed { i, s -> if (i == pos) s.drop(1) else s },
                newState, invariant, transition,
                postCondition, MM
            ).bind()
        }.fix()
    }
}
