package io.jannis.propTest.matchers

import arrow.core.*
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

sealed class Result {
    data class Success(
        val numTest: Int,
        val numDiscarded: Int
    ) : Result()

    data class Failure(
        val numTest: Int,
        val numDiscarded: Int,
        val numShrinks: Int,
        val numShrinkTries: Int,
        val numShrinkFinal: Int,
        val seed: Long,
        val size: Int,
        val exception: Option<Throwable>,
        val output: String,
        val shrinkingSteps: List<ShrinkTestResult<String>>
    ) : Result()

    companion object {
        fun combineSuccess(a: Success, b: Success): Success = Success(
            a.numTest + b.numTest,
            a.numDiscarded + b.numDiscarded
        )

        fun combineSuccessFailure(a: Success, b: Failure): Failure = Failure(
            a.numTest + b.numTest,
            a.numDiscarded + b.numDiscarded,
            b.numShrinks, b.numShrinkTries, b.numShrinkFinal,
            b.seed, b.size, b.exception, b.output, b.shrinkingSteps
        )
    }
}


fun Result.combine(b: Result): Result = when (this) {
    is Result.Success -> when (b) {
        is Result.Success -> Result.combineSuccess(this, b)
        is Result.Failure -> Result.combineSuccessFailure(this, b)
    }
    is Result.Failure -> when (b) {
        is Result.Success -> Result.combineSuccessFailure(b, this)
        is Result.Failure -> this
    }
}

// TODO throw assertion error for kotlintest
inline fun <reified A : Any> runTest(
    arbA: Arbitrary<A> = defArbitrary(),
    config: Config = Config(),
    showA: Show<A> = Show.any(),
    eqA: Eq<A> = Eq.any(),
    noinline discardF: PropTest<A> = { false },
    noinline f: PropTest<A>
): Unit = _runTest(arbA, config, showA, eqA, runSafe(discardF), runSafe(f)).flatMap {
    printResult(
        config,
        it
    )
}.unsafeRunSync()

fun printResult(config: Config, res: Result): IO<Unit> = IO {
    when (res) {
        is Result.Success -> println("Success: Tested ${res.numTest}" + (if (res.numDiscarded > 0) " Discarded: ${res.numDiscarded}" else ""))
        is Result.Failure -> {
            println(
                "Failure: \"${res.output}\" Tested ${res.numTest}" + (if (res.numDiscarded > 0) " Discarded: ${res.numDiscarded}" else "") + res.exception.fold(
                    { "" },
                    { " with \"$it\"" })
            )
            // print shrinking steps
            if (config.printShrinkingOutput) {
                println("Attempt to shrink: \"${res.output}\"")
                val out = res.shrinkingSteps.mapIndexed { i, v -> i toT v }.joinToString("\n") { (i, v) ->
                    "Shrink #${i + 1}: \"${v.value}\" ${if (v.passed) (if (v.discarded) "DISCARDED" else "PASSED") else "FAILED"}" + v.exception.fold(
                        { "" },
                        { " with \"it\"" })
                }
                println(out)
                println(
                    "Shrinking => \"${res.shrinkingSteps.lastOrNull { it.passed.not() }.toOption().fold(
                        { res.output },
                        { it.value })}\" after ${res.numShrinks} successful shrinks and ${res.numShrinkTries} failed shrink tries. ${res.numShrinkFinal} failed ${if (res.numShrinkFinal > 1) "tries" else "try"} after last successful shrink."
                )
            }
        }
    }
}.followedBy(when (res) {
    is Result.Success -> IO.unit
    is Result.Failure -> IO.raiseError(
        AssertionError(
            "\nProperty test failed for: \"${res.output}\"" + res.exception.fold(
                { "" },
                { " with \"$it\"" }
            ) + " after ${res.numTest} ${if (res.numTest > 1) "tries" else "try"}${if (res.numDiscarded > 0) ". Discarded ${res.numDiscarded}" else ""}" + res.shrinkingSteps.lastOrNull { it.passed.not() }.toOption().fold(
                {
                    if (config.maxShrinks > 0) "\nShrunk to: \"${res.output}\"" else ""
                },
                {
                    "\nShrunk to: \"${it.value}\"" + it.exception.fold(
                        { "" },
                        { " with \"$it\"" }
                    ) + " after ${res.shrinkingSteps.size} ${if (res.shrinkingSteps.size > 1) "tries" else "try"}"
                })
        )
    )
}
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

inline fun <reified A : Any> _runTest(
    arbA: Arbitrary<A>,
    config: Config,
    showA: Show<A>,
    eqA: Eq<A>,
    noinline discardF: PropTestIO<A> = { IO.just(false) },
    noinline f: PropTestIO<A>
): IO<Result> = IO.monad().fx {
    val (seed, size) = config.replay.fold({ IO { Random.nextLong() toT 30 }.bind() }, { it })

    val a = arbA.arbitrary().unGen(seed toT size)

    // ugly early returns TODO refractor?
    discardF(a).attempt().bind().fold({
        return@fx Result.Success(1, 1)
    }, {
        if (it) return@fx Result.Success(1, 1)
    })

    val res = f(a).attempt().bind()

    val handleFailure: (Option<Throwable>) -> IO<Result> = { exc ->
        IO.monad().fx {
            val shrinkRes = shrink(config.maxShrinks, arbA, eqA, a, setOf(a), discardF, f).bind().toList()

            val successFullShrinks = shrinkRes.filter { it.passed.not() }.size

            Result.Failure(
                1, 0,
                successFullShrinks, shrinkRes.filter { it.passed }.size,
                shrinkRes.takeLastWhile { it.passed }.size,
                seed, size, exc,
                showA.run { a.show() },
                shrinkRes.map { ShrinkTestResult(it.passed, it.discarded, it.exception, showA.run { it.value.show() }) }
            )
        }.fix()
    }

    res.fold({
        handleFailure(it.some()).bind()
    }, {
        if (it) Result.Success(1, 0)
        else handleFailure(none()).bind()
    })
}.fix()