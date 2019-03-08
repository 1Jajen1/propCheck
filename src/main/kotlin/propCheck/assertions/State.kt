package propCheck.assertions

import arrow.core.Option
import arrow.core.Tuple2
import arrow.core.some
import arrow.core.toT
import arrow.data.StateT
import arrow.data.extensions.list.foldable.foldLeft
import arrow.data.extensions.list.traverse.sequence
import arrow.data.extensions.sequence.foldable.foldLeft
import arrow.data.extensions.statet.monad.monad
import arrow.data.fix
import arrow.effects.IO
import arrow.effects.extensions.io.applicative.applicative
import arrow.effects.extensions.io.concurrent.concurrent
import arrow.effects.extensions.io.monad.monad
import arrow.effects.fix
import arrow.syntax.collections.firstOption
import kotlinx.coroutines.Dispatchers
import propCheck.*
import propCheck.gen.applicative.applicative
import propCheck.gen.monad.monad
import kotlin.system.measureTimeMillis

// precond
// postcond
// invariant

typealias PreCondition<S, A> = (S, A) -> Boolean
typealias PostCondition<S, A, R> = (S, A, R) -> Boolean
typealias Invariant<S> = (S) -> Boolean

// action: A
// state: S

typealias Transition<S, A> = (S, A) -> S

data class StateMachine<S, A, R, SUT>(
    val initialState: S,
    val cmdGen: (S) -> Gen<Option<A>>,
    val preCondition: PreCondition<S, A>,
    val postCondition: PostCondition<S, A, R>,
    val invariant: Invariant<S>,
    val transition: Transition<S, A>,
    val executeAction: (A, SUT) -> IO<R>,
    val sut: () -> IO<SUT>
)

fun <S, A, R, SUT> execSeq(sm: StateMachine<S, A, R, SUT>): Property = forAllShrinkBlind(
    commandsArby(sm, sm.initialState)
) { cmds ->
    idempotentIOProperty(
        IO.monad().binding {
            val results = execSequence(cmds.asSequence(), sm.sut().bind(), sm.executeAction).bind()

            counterexample(
                results.joinToString { "${it.a} -> ${it.b}" }, checkAllActions(
                    results,
                    sm.initialState,
                    sm.transition,
                    sm.invariant,
                    sm.postCondition
                )
            )
        }.fix()
    )
}

fun <A, R, SUT> execSequence(listA: Sequence<A>, sut: SUT, f: (A, SUT) -> IO<R>): IO<Sequence<Tuple2<A, R>>> =
    IO.monad().binding {
        listA.foldLeft(IO.just(emptySequence<Tuple2<A, R>>())) { accIO, a ->
            accIO.flatMap { acc ->
                f(a, sut).map {
                    acc + listOf(a toT it)
                }
            }
        }.bind()
    }.fix()

fun <S, A, R> checkAllActions(
    list: Sequence<Tuple2<A, R>>,
    initS: S,
    transition: Transition<S, A>,
    invariant: Invariant<S>,
    postCondition: PostCondition<S, A, R>
): Boolean {
    fun go(s: S, tup: Tuple2<A, R>, xs: Sequence<Tuple2<A, R>>): Boolean =
        transition(s, tup.a).let { nS ->
            if (postCondition(nS, tup.a, tup.b) && invariant(nS))
                xs.firstOption().fold({ true }, { go(nS, it, xs.drop(1)) })
            else
                false
        }
    return list.firstOption().fold({ true }, { go(initS, it, list.drop(1)) })
}

fun <S, A, R, SUT> commandsArby(
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
                sm.transition(s, v).let {
                    (b && sm.preCondition(it, v) && sm.invariant(it)) toT it
                }
            }.a toT it
        }.filter { it.a }.map { it.b }
    }

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
        // TODO rewrite this and other folds using arrow-recursion once I implement histomorphisms there
        (sequenceOf(Tuple2(Tuple2(true, sm.initialState), emptyList<A>())) + preArb.shrink(pre).map {
            it.foldLeft(true toT sm.initialState) { (b, s), v ->
                sm.transition(s, v).let {
                    (b && sm.preCondition(it, v) && sm.invariant(it)) toT it
                }
            } toT it
        }).filter { it.a.a }.flatMap { (state, shrunkPre) ->
            val (_, preS) = state
                    shrinkList<List<A>> { preArb.shrink(it) }
                        .invoke(a).map {
                            it.filter {
                                it.foldLeft(true toT preS) { (b, s), v ->
                                    sm.transition(s, v).let { nS ->
                                        (b && sm.preCondition(nS, v) && sm.invariant(nS)) toT nS
                                    }
                                }.a
                            }
                        }.map { Tuple2(shrunkPre, it) }
        }
    }
}

fun <S, A, R, SUT> execPar(sm: StateMachine<S, A, R, SUT>, maxThreads: Int = 2): Property = forAllShrinkBlind(
    parArby(sm, Math.max(maxThreads, 2))
) { (prefix, paths) ->
    idempotentIOProperty(
        IO.monad().binding {
            val sut = sm.sut().bind()
            val prefixResult = execSequence(prefix.asSequence(), sut, sm.executeAction).bind()

            val (state, prefixRes) = prefixResult.fold(sm.initialState toT true) { (s, acc), v ->
                val newState = sm.transition(s, v.a)
                newState toT (acc && sm.postCondition(newState, v.a, v.b))
            }
            if (prefixRes.not())
                counterexample("Prefix failed: ${prefixResult.joinToString { "${it.a} -> ${it.b}" }}", prefixRes)
            else {
                val pathResults = IO.concurrent().run {
                    paths.map { list ->
                        IO.unit.continueOn(Dispatchers.Default)
                            .flatMap { execSequence(list.asSequence(), sut, sm.executeAction) }
                    }.sequence(IO.applicative()).bind().fix()
                }
                counterexample(
                    "No possible interleaving found for: \n" +
                            (if (prefix.isNotEmpty()) "Prefix: " + prefixResult.joinToString { "${it.a} -> ${it.b}" } + "\n" else "") +
                            pathResults.filter { it.firstOrNull() != null }.withIndex().joinToString("\n") { (i, v) ->
                                "Path ${i + 1}: " + v.joinToString { "${it.a} -> ${it.b}" }
                            },
                    recurGo(pathResults, state, sm.invariant, sm.transition, sm.postCondition)
                )
            }
        }.fix()
    )
}

fun <S, A, R> recurGo(
    list: List<Sequence<Tuple2<A, R>>>,
    state: S,
    invariant: Invariant<S>,
    transition: Transition<S, A>,
    postCondition: PostCondition<S, A, R>
): Boolean = list.filter { it.firstOrNull() != null }.let {
    it.isEmpty() || it.mapIndexed { i, _ ->
        if (go(it, state, i, invariant, transition, postCondition)) return@let true
        else false
    }.fold(false) { acc, b -> acc || b }
}

fun <S, A, R> go(
    list: List<Sequence<Tuple2<A, R>>>,
    state: S,
    pos: Int,
    invariant: Invariant<S>,
    transition: Transition<S, A>,
    postCondition: PostCondition<S, A, R>
): Boolean = when {
    list[pos].firstOrNull() == null -> true
    else -> {
        val (a, r) = list[pos].first()
        val nS = transition(state, a)
        if ((invariant(nS) && postCondition(nS, a, r)).not()) false
        else recurGo(
            list.mapIndexed { i, s -> if (i == pos) s.drop(1) else s },
            nS,
            invariant,
            transition,
            postCondition
        )
    }
}

// TODO remove because the recurGo function does the same in way less time
fun <A> interleave(listA: Sequence<A>, listB: Sequence<A>, bool: Boolean = true): Sequence<Sequence<A>> = when {
    listA.firstOrNull() == null -> sequenceOf(listB)
    listB.firstOrNull() == null -> sequenceOf(listA)
    else ->
        /* this feels faster, the idea is two parallel threads will usually execute in order and
            next to each other. Thus we alternate between listA and listB early on and only test cases where one thread
            was significantly faster later. Since this is lazy the earlier a check suceeds the better
            Example:
                listA = [1, 2], listB[3, 4] will yield
                [[1, 3, 2, 4], [3, 1, 4, 2], [1, 2, 3, 4], [3, 4, 1, 2]]
                I would consider the last two cases edge cases, this should save time with larger lists
         */
        sequenceOf(listA.first() toT listB.first()).flatMap { (a, b) ->
            if (bool) interleave(listA.drop(1), listB, bool.not()).map {
                sequenceOf(a) + it
            } + interleave(listA, listB.drop(1), bool.not()).map {
                sequenceOf(b) + it
            } else interleave(listA, listB.drop(1), bool.not()).map {
                sequenceOf(b) + it
            } + interleave(listA.drop(1), listB, bool.not()).map {
                sequenceOf(a) + it
            }
        }
    /*
        TODO validate that this is a lot slower
        I mean this should be obvious but test anyway
        if thread 2 starts first we have to check at least n / 2 cases before we get to our case

        sequenceOf(listA.first()).flatMap { a ->
            interleave(listA.drop(1), listB).map {
                sequenceOf(a) + it
            }
        } + sequenceOf(listB.first()).flatMap { b ->
            interleave(listA, listB.drop(1)).map {
                sequenceOf(b) + it
            }
        }
        */
}

fun <S, A> generateCmds(
    size: Int,
    cmdGen: (S) -> Gen<Option<A>>,
    preCondition: PreCondition<S, A>,
    transition: Transition<S, A>,
    invariant: Invariant<S>
): StateT<ForGen, S, List<A>> = StateT.monad<ForGen, S>(Gen.monad()).binding {
    if (size == 0) emptyList()
    else {
        val currState: S = StateT.get<ForGen, S>(Gen.applicative()).bind()
        val newCmd = StateT.liftF<ForGen, S, Option<A>>(Gen.applicative(), cmdGen(currState).suchThat { optCmd ->
            optCmd.fold({ true }, { preCondition(currState, it) })
        }).bind()
        newCmd.fold({ emptyList<A>() }, { a ->
            transition(currState, a).let {
                if (invariant(it)) {
                    StateT.set(Gen.applicative(), it).bind()
                    listOf(a) + generateCmds(
                        size - 1,
                        cmdGen,
                        preCondition,
                        transition,
                        invariant
                    ).bind()
                } else emptyList()
            }
        })
    }
}.fix()
