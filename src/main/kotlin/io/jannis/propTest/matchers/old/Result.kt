package io.jannis.propTest.matchers.old

import arrow.core.Option
import arrow.core.extensions.semigroup
import arrow.core.toOption
import arrow.core.toT
import arrow.data.ListK
import arrow.data.MapK
import arrow.data.extensions.mapk.semigroup.plus
import arrow.data.extensions.mapk.semigroup.semigroup
import arrow.data.k
import arrow.effects.IO
import arrow.effects.extensions.io.monad.followedBy
import arrow.typeclasses.Eq
import arrow.typeclasses.Show
import io.jannis.propTest.Arbitrary
import io.jannis.propTest.defArbitrary

sealed class Result {
    data class Success(
        val numTest: Int,
        val numDiscarded: Int,
        val labelsOut: MapK<ListK<String>, Int> = emptyMap<ListK<String>, Int>().k(),
        val classifiersOut: MapK<String, Int> = emptyMap<String, Int>().k(),
        val tabularsOut: MapK<String, MapK<String, Int>> = emptyMap<String, MapK<String, Int>>().k()
    ) : Result()

    data class Failure(
        val numTest: Int,
        val numDiscarded: Int,
        val labelsOut: MapK<ListK<String>, Int> = emptyMap<ListK<String>, Int>().k(),
        val classifiersOut: MapK<String, Int> = emptyMap<String, Int>().k(),
        val tabularsOut: MapK<String, MapK<String, Int>> = emptyMap<String, MapK<String, Int>>().k(),
        val numShrinks: Int,
        val numShrinkTries: Int,
        val numShrinkFinal: Int,
        val seed: Long,
        val size: Int,
        val exception: Option<Throwable>,
        val output: String,
        val shrinkingSteps: List<ShrinkTestResult<String>>
    ) : Result()
}

fun Result.Success.combine(other: Result): Result = when (other) {
    is Result.Success -> Result.Success(
        numTest + other.numTest,
        numDiscarded + other.numDiscarded,
        labelsOut.plus(Int.semigroup(), other.labelsOut),
        classifiersOut.k().plus(Int.semigroup(), other.classifiersOut.k()),
        tabularsOut.k().plus<String, MapK<String, Int>>(MapK.semigroup(Int.semigroup()), other.tabularsOut.k())
    )
    is Result.Failure -> Result.Failure(
        numTest + other.numTest,
        numDiscarded + other.numDiscarded,
        labelsOut.plus(Int.semigroup(), other.labelsOut),
        classifiersOut.k().plus(Int.semigroup(), other.classifiersOut.k()),
        tabularsOut.k().plus<String, MapK<String, Int>>(MapK.semigroup(Int.semigroup()), other.tabularsOut.k()),
        other.numShrinks, other.numShrinkTries, other.numShrinkFinal,
        other.seed, other.size, other.exception, other.output, other.shrinkingSteps
    )
}

// TODO throw assertion error for kotlintest
inline fun <reified A : Any> runTest(
    arbA: Arbitrary<A> = defArbitrary(),
    config: Config = Config(),
    showA: Show<A> = Show.any(),
    eqA: Eq<A> = Eq.any(),
    noinline f: PropTest<A>
): Unit = _runTest(arbA, config, showA, eqA, f.toProperty()).flatMap {
    printResultAndThrowIfNeeded(
        config,
        it
    )
}.unsafeRunSync()

fun Int.toPercentage(max: Int): String =
    "%.2f".format(100 * (this.toDouble() / max.toDouble()))

fun printResultAndThrowIfNeeded(config: Config, res: Result): IO<Unit> = IO {
    when (res) {
        is Result.Success -> {
            println("Success: Tested ${res.numTest}" + (if (res.numDiscarded > 0) " Discarded: ${res.numDiscarded}" else "") + (
                    if (res.classifiersOut.isNotEmpty()) " (" + res.classifiersOut.entries.toList().sortedByDescending { it.value }.joinToString { "${it.value.toPercentage(res.numTest)}% ${it.key}" } + ")" else ""
                    ))
            if (res.labelsOut.isNotEmpty()) {
                println(
                    "\n" + res.labelsOut.entries.toList().sortedByDescending { it.value }.joinToString("\n") {
                        "${it.value.toPercentage(res.numTest)}% ${it.key.joinToString()}"
                    }
                )
            }

            if (res.tabularsOut.isNotEmpty()) {
                println(
                    "\n" + res.tabularsOut.entries.toList().joinToString {
                        "${it.key}:\n" +
                                it.value.entries.toList().sortedByDescending { it.value }.joinToString("\n") { "${it.value.toPercentage(res.numTest)}% ${it.key}" }
                    }
                )
            }
        }
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