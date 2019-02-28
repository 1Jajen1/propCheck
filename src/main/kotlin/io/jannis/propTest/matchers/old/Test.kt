package io.jannis.propTest.matchers.old

import arrow.core.*
import arrow.core.extensions.semigroup
import arrow.data.ListK
import arrow.data.MapK
import arrow.data.extensions.mapk.semigroup.plus
import arrow.data.extensions.mapk.semigroup.semigroup
import arrow.data.extensions.sequence.applicative.map
import arrow.data.k
import arrow.effects.IO
import arrow.effects.extensions.io.monad.followedBy
import arrow.effects.extensions.io.monad.monad
import arrow.effects.extensions.io.monadThrow.monadThrow
import arrow.effects.fix
import arrow.syntax.collections.firstOption
import arrow.typeclasses.Eq
import arrow.typeclasses.Show
import io.jannis.propTest.Arbitrary
import io.jannis.propTest.defArbitrary
import kotlin.random.Random

typealias PropTest<A> = (A) -> Boolean
typealias PropTestIO<A> = (A) -> IO<Boolean>

data class Config(
    val replay: Option<Tuple2<Long, Int>> = none(),
    val maxSuccess: Int = 100,
    val maxDiscardRatio: Int = 10,
    val maxShrinks: Int = Int.MAX_VALUE,
    val printShrinkingOutput: Boolean = true
)

fun <A, B> runSafe(f: (A) -> B): (A) -> IO<B> = {
    IO.monadThrow().bindingCatch { f(it) }.fix()
}

data class ShrinkTestResult<A>(
    val passed: Boolean,
    val discarded: Boolean,
    val exception: Option<Throwable>,
    val value: A
)

fun <A> shrink(
    maxShrinks: Int,
    arbA: Arbitrary<A>,
    eqA: Eq<A>,
    toShrink: A,
    tested: Set<A> = emptySet(),
    discardF: PropTestIO<A> = { IO.just(false) },
    f: PropTestIO<A>
): IO<Sequence<ShrinkTestResult<A>>> = IO.monad().fx {
    if (maxShrinks > 0)
        arbA.shrink(toShrink).filter { tested.contains(it).not() }.k().map { newA ->
            discardF(newA).attempt().flatMap {
                IO.monad().fx {
                    it.fold({ ShrinkTestResult(true, true, none(), newA) }, {
                        if (it) ShrinkTestResult(true, true, none(), newA)
                        else {
                            f(newA).attempt().map {
                                it.fold({
                                    ShrinkTestResult(false, false, it.some(), newA)
                                }, {
                                    ShrinkTestResult(it, false, none(), newA)
                                })
                            }.bind()
                        }
                    })
                }.fix()
            }
        }.map {
            // FIXME workaround, will change if possible
            it.unsafeRunSync()
        }.filter { it.discarded.not() }.take(maxShrinks).distinct().let { seq ->
            seq.withIndex().firstOption { it.value.passed.not() }.fold({ seq }, {
                seq.take(it.index + 1) + shrink(
                    maxShrinks - it.index - 1,
                    arbA,
                    eqA,
                    it.value.value,
                    seq.take(it.index + 1).map { it.value }.toSet() + tested,
                    discardF,
                    f
                ).bind()
            })
        }
    else emptySequence<ShrinkTestResult<A>>().k()
}.fix()

data class Property<A> internal constructor(
    val condition: (A) -> Boolean,
    val discard: (A) -> Boolean = { false },
    val labels: List<(A) -> String> = emptyList(),
    val classifiers: List<Tuple2<String, (A) -> Boolean>> = emptyList(),
    val tabulars: List<Tuple2<String, (A) -> String>> = emptyList()
)

fun <A>((A) -> Boolean).toProperty(): Property<A> =
    Property(condition = this)

fun <A> discard(discard: (A) -> Boolean, test: (A) -> Boolean): Property<A> =
    Property(condition = test, discard = discard)

inline fun <reified A : Any> _runTest(
    arbA: Arbitrary<A>,
    config: Config,
    showA: Show<A>,
    eqA: Eq<A>,
    property: Property<A>
): IO<Result> = IO.monad().fx {
    val (seed, size) = config.replay.fold({ IO { Random.nextLong() toT 30 }.bind() }, { it })

    val a = arbA.arbitrary().unGen(seed toT size)

    // ugly early returns TODO refractor?
    runSafe(property.discard).invoke(a).attempt().bind().fold({
        return@fx Result.Success(1, 1)
    }, {
        if (it) return@fx Result.Success(1, 1)
    })

    val res = runSafe(property.condition).invoke(a).attempt().bind()

    val labelsOut: Map<ListK<String>, Int> = mapOf(*property.labels.map { listOf(it(a)).k() to 1 }.toTypedArray())
    val classifiersOut = mapOf(*property.classifiers.filter { it.b(a) }.map { it.a to 1 }.toTypedArray())
    val tabularsOut = mapOf(*property.tabulars.map { it.a to mapOf(it.b(a) to 1).k() }.toTypedArray())

    val handleFailure: (Option<Throwable>) -> IO<Result> = { exc ->
        IO.monad().fx {
            val shrinkRes = shrink(
                config.maxShrinks,
                arbA,
                eqA,
                a,
                setOf(a),
                runSafe(property.discard),
                runSafe(property.condition)
            ).bind().toList()

            val successFullShrinks = shrinkRes.filter { it.passed.not() }.size

            Result.Failure(
                1, 0,
                labelsOut.k(), classifiersOut.k(), tabularsOut.k(),
                successFullShrinks, shrinkRes.filter { it.passed }.size,
                shrinkRes.takeLastWhile { it.passed }.size,
                seed, size, exc,
                showA.run { a.show() },
                shrinkRes.map {
                    ShrinkTestResult(
                        it.passed,
                        it.discarded,
                        it.exception,
                        showA.run { it.value.show() })
                }
            )
        }.fix()
    }

    res.fold({
        handleFailure(it.some()).bind()
    }, {
        if (it) Result.Success(1, 0, labelsOut.k(), classifiersOut.k(), tabularsOut.k())
        else handleFailure(none()).bind()
    })
}.fix()