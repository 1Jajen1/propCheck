package propCheck

import arrow.core.*
import arrow.core.extensions.eval.monad.monad
import arrow.data.StateT
import arrow.data.extensions.list.foldable.foldLeft
import arrow.data.extensions.list.foldable.foldRight
import arrow.data.extensions.statet.monad.monad
import arrow.data.fix
import arrow.effects.IO
import arrow.effects.extensions.io.monad.monad
import arrow.effects.fix
import propCheck.assertions.*
import propCheck.gen.applicative.applicative
import propCheck.gen.monad.monad
import java.util.concurrent.Callable
import java.util.concurrent.Executors
import java.util.concurrent.ThreadFactory



typealias PreCondition<S, A> = (S, A) -> Boolean
typealias PostCondition<S, A, R, PROP> = (S, A, R) -> PROP
typealias Invariant<S> = (S) -> Boolean
typealias Transition<S, A> = (S, A) -> S

data class StateMachine<S, A, R, SUT>(
    val initialState: S,
    val cmdGen: (S) -> Gen<Option<A>>,
    val preCondition: PreCondition<S, A>,
    val invariant: Invariant<S>,
    val transition: Transition<S, A>,
    val executeAction: (A, SUT) -> IO<R>,
    val sut: () -> IO<SUT>
)

@PublishedApi
internal fun <S, A, R, SUT> commandsArby(
    sm: StateMachine<S, A, R, SUT>,
    initialState: S,
    shrinker: (A) -> Sequence<A> = { emptySequence() }
) =
    Arbitrary(
        Gen.sized {
            generateCmds(it, sm.cmdGen, sm.preCondition, sm.transition, sm.invariant)
                .runA(Gen.monad(), initialState)
                .fix()
        }
    ) { fail ->
        shrinkList<A> { shrinker(it) }.invoke(fail).map {
            it.foldLeft(true toT sm.initialState) { (b, s), v ->
                (b && sm.preCondition(s, v) && sm.invariant(s)) toT sm.transition(s, v)
            }.a toT it
        }.filter { it.a }.map { it.b }
    }

@PublishedApi
internal fun <S, A> generateCmds(
    size: Int,
    cmdGen: (S) -> Gen<Option<A>>,
    preCondition: PreCondition<S, A>,
    transition: Transition<S, A>,
    invariant: Invariant<S>
): StateT<ForGen, S, List<A>> = StateT.monad<ForGen, S>(Gen.monad()).binding {
    if (size == 0) emptyList()
    else {
        val currState: S = StateT.get<ForGen, S>(Gen.applicative()).bind()
        StateT.liftF<ForGen, S, Option<A>>(Gen.applicative(), cmdGen(currState).suchThat { optCmd ->
            optCmd.fold({ true }, { preCondition(currState, it) && invariant(transition(currState, it)) })
        }).bind().fold({
            emptyList<A>()
        }, {
            StateT.set(Gen.applicative(), transition(currState, it)).bind()
            listOf(it) + generateCmds(size - 1, cmdGen, preCondition, transition, invariant).bind()
        })
    }
}.fix()

@PublishedApi
internal fun <A, R, SUT> executeActions(
    actions: List<A>,
    sut: SUT,
    executeAction: (A, SUT) -> IO<R>
): IO<List<Tuple2<A, R>>> =
    actions.foldLeft(IO.just(emptyList())) { accIO, v ->
        accIO.flatMap {
            executeAction(v, sut).map { r ->
                it + listOf(Tuple2(v, r))
            }
        }
    }

@PublishedApi
internal fun <S, A, R, PROP> checkResults(
    res: List<Tuple2<A, R>>,
    state: S,
    postCondition: PostCondition<S, A, R, PROP>,
    invariant: Invariant<S>,
    testable: Testable<PROP>,
    transition: Transition<S, A>
): Tuple2<Eval<Property>, S> =
    res.foldLeft(
        Eval.later { Boolean.testable().run { true.property() } } toT state
    ) { (prop, s), (a, r) ->
        Eval.later {
            and(
                and(
                    Boolean.testable().run { invariant(s).property() },
                    Eval.later { testable.run { postCondition(s, a, r).property() } }
                ),
                prop
            )
        } toT transition(s, a)
    }

inline fun <S, A, R, SUT, reified PROP> execSeq(
    sm: StateMachine<S, A, R, SUT>,
    testable: Testable<PROP> = defTestable(),
    noinline postCondition: PostCondition<S, A, R, PROP>
): Property = forAllShrinkBlind(
    commandsArby(sm, sm.initialState)
) { cmds ->
    idempotentIOProperty(
        IO.monad().binding {
            val sut = sm.sut().bind()
            val results = executeActions(cmds, sut, sm.executeAction).bind()

            checkResults(results, sm.initialState, postCondition, sm.invariant, testable, sm.transition).a.value()
        }.fix()
    )
}

// ------------ parallel
fun <S, A, R, SUT> parArby(sm: StateMachine<S, A, R, SUT>, maxThreads: Int) = commandsArby(
    sm,
    sm.initialState
).let { preArb ->
    Arbitrary(
        Gen.monad().binding {
            val prefix = preArb.arbitrary().bind()
            val s = prefix.foldLeft(sm.initialState) { s, a -> sm.transition(s, a) }
            val pathArb = commandsArby(sm, s)
            val threads = Gen.choose(2 toT maxThreads, Int.random()).bind()
            Tuple2(
                prefix,
                (0..(threads - 1)).map {
                    pathArb.arbitrary().resize(Gen.choose(0 toT Gen.getSize().bind(), Int.random()).bind() / threads)
                        .bind()
                }
            )
        }.fix()
    ) { (pre, a) ->
        (sequenceOf(Tuple2(Tuple2(true, sm.initialState), emptyList<A>())) + preArb.shrink(pre).map {
            it.foldLeft(true toT sm.initialState) { (b, s), v ->
                (b && sm.preCondition(s, v) && sm.invariant(s)) toT sm.transition(s, v)
            } toT it
        }).filter { it.a.a }.flatMap { (state, shrunkPre) ->
            val (_, preS) = state
            shrinkList<List<A>> { preArb.shrink(it) }
                .invoke(a).map {
                    it.filter {
                        it.foldLeft(true toT preS) { (b, s), v ->
                            (b && sm.preCondition(s, v) && sm.invariant(s)) toT sm.transition(s, v)
                        }.a
                    }
                }.map { Tuple2(shrunkPre, it) }
        }
    }
}

inline fun <S, A, R, SUT, reified PROP> execPar(
    sm: StateMachine<S, A, R, SUT>,
    maxThreads: Int = 2,
    testable: Testable<PROP> = defTestable(),
    noinline postCondition: PostCondition<S, A, R, PROP>
) = _execPar(sm, maxThreads, testable, postCondition)

private val pool = Executors.newCachedThreadPool { r ->
    Executors.defaultThreadFactory().newThread(r).apply {
        isDaemon = true
    }
}

/*
Using this directly as an inline function fucked up the compiler
TODO try again when I update kotlin
 */
@PublishedApi
internal fun <S, A, R, SUT, PROP> _execPar(
    sm: StateMachine<S, A, R, SUT>,
    maxThreads: Int = 2,
    testable: Testable<PROP>,
    postCondition: PostCondition<S, A, R, PROP>
): Property = forAllShrinkBlind(
    parArby(sm, Math.max(maxThreads, 2))
) { (prefix, paths) ->
    idempotentIOProperty(
        IO.monad().binding {
            val sut = sm.sut().bind()
            val prefixResult = executeActions(prefix, sut, sm.executeAction).bind()

            val (prefixRes, state) = checkResults(
                prefixResult,
                sm.initialState,
                postCondition,
                sm.invariant,
                testable,
                sm.transition
            )

            // I don't really like this next bit, however I could not force IO to start in different threads
            //  before and I really need them to execute in parallel
            val pathResults = pool.invokeAll(
                paths.map { list ->
                    Callable {
                        executeActions(list, sut, sm.executeAction).unsafeRunSync().asSequence()
                    }
                }
            ).map { it.get() }

            counterexample(
                "No possible interleaving found for: \n" +
                        (if (prefix.isNotEmpty()) "Prefix: " + prefixResult.joinToString { "${it.a} -> ${it.b}" } + "\n" else "") +
                        pathResults.filter { it.firstOrNull() != null }.withIndex().joinToString("\n") { (i, v) ->
                            "Path ${i + 1}: " + v.joinToString { "${it.a} -> ${it.b}" }
                        },
                and(
                    prefixRes.value(),
                    Eval.later {
                        recurGo(pathResults, state, sm.invariant, sm.transition, testable, postCondition)
                    }
                )
            )
        }.fix()
    )
}

internal fun <S, A, R, PROP> recurGo(
    list: List<Sequence<Tuple2<A, R>>>,
    state: S,
    invariant: Invariant<S>,
    transition: Transition<S, A>,
    testable: Testable<PROP>,
    postCondition: PostCondition<S, A, R, PROP>
): Property = list.filter { it.firstOrNull() != null }.let {
    or(
        Boolean.testable().run { it.isEmpty().property() },
        Eval.later {
            it.mapIndexed { i, _ ->
                Eval.later { go(it, state, i, invariant, transition, testable, postCondition) }
            }.foldRight(Eval.now(Boolean.testable().run { false.property() })) { v, acc ->
                Eval.monad().binding {
                    or(
                        v.bind(),
                        acc
                    )
                }.fix()
            }.value()
        }
    )
}

internal fun <S, A, R, PROP> go(
    list: List<Sequence<Tuple2<A, R>>>,
    state: S,
    pos: Int,
    invariant: Invariant<S>,
    transition: Transition<S, A>,
    testable: Testable<PROP>,
    postCondition: PostCondition<S, A, R, PROP>
): Property = when {
    list[pos].firstOrNull() == null -> Boolean.testable().run { true.property() }
    else -> {
        val (a, r) = list[pos].first()
        val newState = transition(state, a)
        and(
            and(
                Boolean.testable().run { invariant(newState).property() },
                Eval.later { testable.run { postCondition(state, a, r).property() } }
            ),
            Eval.later {
                recurGo(
                    list.mapIndexed { i, s -> if (i == pos) s.drop(1) else s },
                    newState,
                    invariant,
                    transition,
                    testable,
                    postCondition
                )
            }
        )
    }
}