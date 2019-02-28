package io.jannis.propTest.assertions

import arrow.core.*
import arrow.core.extensions.monoid
import arrow.core.extensions.semigroup
import arrow.data.MapK
import arrow.data.extensions.list.foldable.fold
import arrow.data.extensions.list.foldable.sequence_
import arrow.data.extensions.list.foldable.traverse_
import arrow.data.extensions.list.traverse.sequence
import arrow.data.extensions.list.traverse.traverse
import arrow.data.extensions.listk.monoid.monoid
import arrow.data.extensions.mapk.foldable.combineAll
import arrow.data.extensions.mapk.semigroup.plus
import arrow.data.extensions.mapk.semigroup.semigroup
import arrow.data.extensions.sequence.foldable.isEmpty
import arrow.data.k
import arrow.effects.ForIO
import arrow.effects.IO
import arrow.effects.Ref
import arrow.effects.extensions.io.applicative.applicative
import arrow.effects.extensions.io.monad.flatMap
import arrow.effects.extensions.io.monad.followedBy
import arrow.effects.extensions.io.monad.monad
import arrow.effects.extensions.io.monadDefer.monadDefer
import arrow.effects.fix
import arrow.extension
import io.jannis.propTest.Gen
import io.jannis.propTest.assertions.testresult.testable.testable
import io.jannis.propTest.fix
import io.jannis.propTest.matchers.old.toPercentage
import kotlin.random.Random

sealed class Result {
    data class Success(
        val labels: MapK<String, Int>,
        val classes: MapK<String, Int>,
        val tables: MapK<String, MapK<String, Int>>,
        val numTests: Int,
        val numDiscardedTests: Int,
        val output: String
    ) : Result()

    data class Failure(
        val usedSeed: Long,
        val usedSize: Int,
        val numTests: Int,
        val numDiscardedTests: Int,
        val numShrinks: Int,
        val numShrinkTries: Int,
        val numShrinkFinal: Int,
        val output: String,
        val reason: String,
        val exception: Option<Throwable>,
        val failingTestCase: List<String>,
        val failingLabels: List<String>,
        val failingClasses: List<String>
    ) : Result()

    data class NoExpectedFailure(
        val labels: MapK<String, Int>,
        val classes: MapK<String, Int>,
        val tables: MapK<String, MapK<String, Int>>,
        val numTests: Int,
        val numDiscardedTests: Int,
        val output: String
    ) : Result()

    data class GivenUp(
        val numTests: Int,
        val numDiscardedTests: Int,
        val output: String,
        val classes: MapK<String, Int>,
        val labels: MapK<String, Int>,
        val tables: MapK<String, MapK<String, Int>>
    ) : Result()
}

data class Args(
    val replay: Option<Tuple2<Long, Int>> = none(),
    val maxSuccess: Int = 100,
    val maxDiscardRatio: Int = 10,
    val maxSize: Int = 100,
    val verbose: Boolean = true,
    val maxShrinks: Int = Int.MAX_VALUE
)

data class State(
    val output: Ref<ForIO, String>,
    val maxSuccess: Int,
    val coverageConfidence: Option<Confidence>,
    val maxDiscardRatio: Int,
    val computeSize: (Int) -> (Int) -> Int,
    val maxShrinks: Int,
    val numSuccessTests: Int,
    val numDiscardedTests: Int,
    val numRecentlyDiscardedTests: Int,
    val labels: MapK<String, Int>,
    val classes: MapK<String, Int>,
    val tables: MapK<String, MapK<String, Int>>,
    val expected: Boolean,
    val numSuccessShrinks: Int,
    val numTryShrinks: Int,
    val numTotTryShrinks: Int,
    val randomSeed: Long,
    val requiredCoverage: MapK<Tuple2<Option<String>, String>, Double>
)

data class Confidence(
    val certainty: Double = Math.pow(10.0, 9.0),
    val tolerance: Double = 0.9
)

inline class Property(val unProperty: Gen<Prop>) {
    companion object
}

inline class Prop(val unProp: Rose<TestResult>)

interface Testable<A> {
    fun A.property(): Property
}

interface BooleanTestable : Testable<Boolean> {
    override fun Boolean.property(): Property = TestResult.testable().run {
        liftBoolean(this@property).property()
    }
}

fun Boolean.Companion.testable(): Testable<Boolean> = object : BooleanTestable {}

@extension
interface TestResultTestable : Testable<TestResult> {
    override fun TestResult.property(): Property =
        Property(Gen.monad().just(Prop(protectResults(Rose.just(this)))).fix())
}

@extension
interface GenTestable<A> : Testable<Gen<A>> {
    fun TA(): Testable<A>
    override fun Gen<A>.property(): Property = Property(
        Gen.monad().fx {
            again(TA()).invoke(this@property.bind()).unProperty.bind()
        }.fix()
    )
}

@extension
interface PropertyTestable : Testable<Property> {
    override fun Property.property(): Property = Property(
        Gen.monad().fx {
            this@property.unProperty.bind()
        }.fix()
    )
}

// combinators
fun propCheck(
    args: Args = Args(),
    f: () -> Property
): IO<Result> = IO.monad().fx {
    val randSeed = args.replay.fold({
        IO { Random.nextLong() }
    }, { IO.just(it.a) }).bind()

    fun roundTo(n: Int, m: Int): Int = (n / m) * m

    val computeSizeA: (Int) -> (Int) -> Int = { n ->
        { d ->
            if (
                roundTo(n, args.maxSize) + args.maxSize <= args.maxSuccess ||
                n >= args.maxSuccess || args.maxSuccess.rem(args.maxSize) == 0
            )
                Math.min(n.rem(args.maxSize) + d / 10, args.maxSize)
            else
                Math.min(
                    (n.rem(args.maxSize) * args.maxSize / (args.maxSuccess.rem(args.maxSize)) + d / 10),
                    args.maxSize
                )
        }
    }

    fun at0(f: (Int) -> (Int) -> Int, s: Int): (Int) -> (Int) -> Int = { n ->
        { d ->
            when (n toT d) {
                Tuple2(0, 0) -> s
                else -> f(n).invoke(d)
            }
        }
    }

    val state = State(
        maxSuccess = args.maxSuccess,
        coverageConfidence = none(),
        maxDiscardRatio = args.maxDiscardRatio,
        computeSize = args.replay.fold({
            computeSizeA
        }, {
            at0(computeSizeA, it.b)
        }),
        maxShrinks = args.maxShrinks,
        numSuccessTests = 0,
        numDiscardedTests = 0,
        numRecentlyDiscardedTests = 0,
        labels = emptyMap<String, Int>().k(),
        classes = emptyMap<String, Int>().k(),
        tables = emptyMap<String, MapK<String, Int>>().k(),
        expected = true,
        numTryShrinks = 0,
        numSuccessShrinks = 0,
        numTotTryShrinks = 0,
        randomSeed = randSeed,
        requiredCoverage = emptyMap<Tuple2<Option<String>, String>, Double>().k(),
        output = Ref.of("", IO.monadDefer()).bind()
    )
    runTest(state, f()).bind()
}.fix()

fun runTest(
    state: State,
    prop: Property
): IO<Result> =
    if (state.numSuccessTests >= state.maxSuccess && state.coverageConfidence.isEmpty())
        doneTesting(state, prop)
    else if (state.numDiscardedTests >= state.maxDiscardRatio * Math.max(state.numSuccessTests, state.maxSuccess))
        giveUpTesting(state, prop)
    else
        runATest(state, prop)

fun doneTesting(state: State, prop: Property): IO<Result> = IO.monad().fx {
    if (state.expected) {
        state.output.update {
            it + "+++ OK, passed ${showTestCount(state)}"
        }.bind()

        putSuccessStr(state).bind()

        val output = state.output.get().bind()

        Result.Success(
            numTests = state.numSuccessTests,
            numDiscardedTests = state.numDiscardedTests,
            output = output,
            classes = state.classes,
            labels = state.labels,
            tables = state.tables
        )
    } else {
        state.output.updateAndGet {
            it + "*** Failed! Passed ${showTestCount(state)} (expected Failure)"
        }.bind()

        putSuccessStr(state).bind()

        val output = state.output.get().bind()

        Result.NoExpectedFailure(
            numTests = state.numSuccessTests,
            numDiscardedTests = state.numDiscardedTests,
            output = output,
            classes = state.classes,
            labels = state.labels,
            tables = state.tables
        )
    }
}.fix()

fun giveUpTesting(state: State, prop: Property): IO<Result> = IO.monad().fx {
    state.output.updateAndGet {
        it + "*** Gave up! Passed only ${showTestCount(state)}"
    }.bind()

    putSuccessStr(state).bind()

    val output = state.output.get().bind()

    Result.GivenUp(
        numTests = state.numSuccessTests,
        numDiscardedTests = state.numDiscardedTests,
        output = output,
        classes = state.classes,
        labels = state.labels,
        tables = state.tables
    )
}.fix()

fun runATest(state: State, prop: Property): IO<Result> = IO.monad().fx {
    // TODO look into actual splittable random gens...
    val (rand1, rand2) = Random(state.randomSeed).let { it.nextLong() toT it.nextLong() }

    val size = state.computeSize(state.numSuccessTests)(state.numRecentlyDiscardedTests)

    val (res0, ts) = when (val it = protectRose(reduceRose(prop.unProperty.unGen(rand1 toT size).unProp)).bind()) {
        is Rose.IORose -> throw IllegalStateException("Should not happend")
        is Rose.MkRose -> it.res toT it.shrunk
    }

    val res = callbackPostTest(state, res0).bind()

    val newState = State(
        coverageConfidence = state.coverageConfidence, // TODO
        maxSuccess = res.optionNumOfTests.getOrElse { state.maxSuccess },
        tables = res.tables.foldRight(state.tables) { (k, l), acc ->
            acc.k()
                .plus<String, MapK<String, Int>>(MapK.semigroup(Int.semigroup()), mapOf(k to (mapOf(l to 1)).k()).k())
        },
        expected = res.expected,
        labels = mapOf(*res.labels.map { it to 1 }.toTypedArray()).k().plus(Int.semigroup(), state.labels),
        classes = mapOf(*res.classes.map { it to 1 }.toTypedArray()).k().plus(Int.semigroup(), state.classes),
        requiredCoverage = state.requiredCoverage, // TODO
        // keep rest
        numTotTryShrinks = state.numTotTryShrinks,
        maxShrinks = state.maxShrinks,
        numSuccessShrinks = state.numSuccessShrinks,
        numTryShrinks = state.numTryShrinks,
        numRecentlyDiscardedTests = state.numRecentlyDiscardedTests,
        numDiscardedTests = state.numDiscardedTests,
        numSuccessTests = state.numSuccessTests,
        computeSize = state.computeSize,
        maxDiscardRatio = state.maxDiscardRatio,
        randomSeed = state.randomSeed,
        output = state.output
    )

    fun cont(nState: State, br: (State, Property) -> IO<Result>): IO<Result> = if (res.abort)
        br(nState, prop)
    else
        runTest(nState, prop)

    when (res.ok) {
        true.some() -> {
            cont(
                State(
                    numSuccessTests = newState.numSuccessTests + 1,
                    numRecentlyDiscardedTests = 0,
                    randomSeed = rand2,
                    // copy rest
                    maxDiscardRatio = newState.maxDiscardRatio,
                    computeSize = newState.computeSize,
                    numDiscardedTests = newState.numDiscardedTests,
                    numTryShrinks = newState.numTryShrinks,
                    numSuccessShrinks = newState.numSuccessShrinks,
                    maxShrinks = newState.maxShrinks,
                    numTotTryShrinks = newState.numTotTryShrinks,
                    requiredCoverage = newState.requiredCoverage,
                    coverageConfidence = newState.coverageConfidence,
                    classes = newState.classes,
                    labels = newState.labels,
                    expected = newState.expected,
                    maxSuccess = newState.maxSuccess,
                    tables = newState.tables,
                    output = state.output
                ), ::doneTesting
            )
        }
        false.some() -> {
            IO.monad().fx {
                val (numShrinks, totFailed, lastFailed, nRes) = foundFailure(newState, res, ts).bind()
                val output = newState.output.get().bind()
                if (nRes.expected.not())
                    Result.Success(
                        numTests = newState.numSuccessTests + 1,
                        numDiscardedTests = newState.numDiscardedTests,
                        output = output,
                        tables = newState.tables,
                        labels = newState.labels,
                        classes = newState.classes
                    )
                else
                    Result.Failure(
                        usedSeed = state.randomSeed,
                        output = output,
                        numDiscardedTests = newState.numDiscardedTests,
                        numTests = newState.numSuccessTests + 1,
                        exception = nRes.exception,
                        reason = nRes.reason,
                        failingClasses = nRes.classes,
                        failingLabels = nRes.labels,
                        failingTestCase = nRes.testCase,
                        numShrinkFinal = lastFailed,
                        numShrinks = numShrinks,
                        numShrinkTries = totFailed,
                        usedSize = size
                    )
            }.fix()
        }
        None -> {
            cont(
                State(
                    numDiscardedTests = newState.numDiscardedTests + 1,
                    numRecentlyDiscardedTests = newState.numRecentlyDiscardedTests + 1,
                    randomSeed = rand2,
                    // copy rest
                    maxDiscardRatio = newState.maxDiscardRatio,
                    computeSize = newState.computeSize,
                    numTryShrinks = newState.numTryShrinks,
                    numSuccessShrinks = newState.numSuccessShrinks,
                    maxShrinks = newState.maxShrinks,
                    numTotTryShrinks = newState.numTotTryShrinks,
                    requiredCoverage = newState.requiredCoverage,
                    coverageConfidence = newState.coverageConfidence,
                    classes = newState.classes,
                    labels = newState.labels,
                    expected = newState.expected,
                    maxSuccess = newState.maxSuccess,
                    tables = newState.tables,
                    numSuccessTests = newState.numSuccessTests,
                    output = state.output
                ), ::giveUpTesting
            )
        }
        else -> throw IllegalStateException("Not possible")
    }.bind()
}.fix()

typealias FailureResult = IO<Tuple4<Int, Int, Int, TestResult>>

fun foundFailure(state: State, res: TestResult, ts: Sequence<Rose<TestResult>>): FailureResult =
    localMin(
        State(
            numTryShrinks = 0,
            // copy rest
            numSuccessTests = state.numSuccessTests,
            randomSeed = state.randomSeed,
            numRecentlyDiscardedTests = state.numRecentlyDiscardedTests,
            numDiscardedTests = state.numDiscardedTests,
            tables = state.tables,
            maxSuccess = state.maxSuccess,
            expected = state.expected,
            labels = state.labels,
            classes = state.classes,
            coverageConfidence = state.coverageConfidence,
            requiredCoverage = state.requiredCoverage,
            numTotTryShrinks = state.numTotTryShrinks,
            maxShrinks = state.maxShrinks,
            numSuccessShrinks = state.numSuccessShrinks,
            computeSize = state.computeSize,
            maxDiscardRatio = state.maxDiscardRatio,
            output = state.output
        ),
        res, ts
    )

fun localMin(state: State, res: TestResult, ts: Sequence<Rose<TestResult>>): FailureResult =
    if (state.numSuccessShrinks + state.numTotTryShrinks >= state.maxShrinks)
        localMinFound(state, res)
    else
        _localMin(state, res, ts)

internal fun _localMin(state: State, res: TestResult, ts: Sequence<Rose<TestResult>>): FailureResult =
    when (ts.isEmpty()) {
        true -> localMinFound(state, res)
        else -> IO.monad().fx {
            val (nRes0, nTs) = when (val it = protectRose(reduceRose(ts.first())).bind()) {
                is Rose.IORose -> throw IllegalStateException("Should never happen")
                is Rose.MkRose -> it.res toT it.shrunk
            }

            val nRes = callbackPostTest(state, nRes0).bind()

            if (nRes.ok == false.some())
                localMin(
                    State(
                        numSuccessShrinks = state.numSuccessShrinks + 1,
                        numTryShrinks = 0,
                        // copy
                        maxDiscardRatio = state.maxDiscardRatio,
                        computeSize = state.computeSize,
                        maxShrinks = state.maxShrinks,
                        numTotTryShrinks = state.numTotTryShrinks,
                        requiredCoverage = state.requiredCoverage,
                        coverageConfidence = state.coverageConfidence,
                        classes = state.classes,
                        labels = state.labels,
                        expected = state.expected,
                        maxSuccess = state.maxSuccess,
                        tables = state.tables,
                        numDiscardedTests = state.numDiscardedTests,
                        numRecentlyDiscardedTests = state.numRecentlyDiscardedTests,
                        randomSeed = state.randomSeed,
                        numSuccessTests = state.numSuccessTests,
                        output = state.output
                    ), nRes, nTs
                ).bind()
            else
                localMin(
                    State(
                        numTryShrinks = state.numTryShrinks + 1,
                        numTotTryShrinks = state.numTotTryShrinks + 1,
                        // copy
                        maxDiscardRatio = state.maxDiscardRatio,
                        computeSize = state.computeSize,
                        maxShrinks = state.maxShrinks,
                        numSuccessShrinks = state.numSuccessShrinks,
                        requiredCoverage = state.requiredCoverage,
                        coverageConfidence = state.coverageConfidence,
                        classes = state.classes,
                        labels = state.labels,
                        expected = state.expected,
                        maxSuccess = state.maxSuccess,
                        tables = state.tables,
                        numDiscardedTests = state.numDiscardedTests,
                        numRecentlyDiscardedTests = state.numRecentlyDiscardedTests,
                        randomSeed = state.randomSeed,
                        numSuccessTests = state.numSuccessTests,
                        output = state.output
                    ), res, ts.drop(1)
                ).bind()
        }.fix()
    }

fun localMinFound(state: State, res: TestResult): FailureResult = IO.monad().fx {
    failureReason(state, res).reversed().map { s ->
        state.output.update {
            "$it$s\n"
        }
    }.sequence_(IO.applicative()).bind()
    callbackPostFinalFailure(state, res).bind()
    Tuple4(state.numSuccessShrinks, state.numTotTryShrinks - state.numTryShrinks, state.numTryShrinks, res)
}.fix()

fun callbackPostTest(state: State, res: TestResult): IO<TestResult> =
    res.callbacks.filter { it is Callback.PostTest }.traverse(IO.applicative()) {
        (it as Callback.PostTest).fn(state, res)
    }.fix().followedBy(IO.just(res))

fun callbackPostFinalFailure(state: State, res: TestResult): IO<Unit> =
        res.callbacks.filter { it is Callback.PostFinalFailure }.traverse(IO.applicative()) {
            (it as Callback.PostFinalFailure).fn(state, res)
        }.fix().followedBy(IO.unit)

fun showTestCount(state: State): String =
    state.numSuccessTests.number("test") + (if (state.numDiscardedTests > 0) "; ${state.numDiscardedTests} discarded" else "")

fun Int.number(str: String): String = "$this $str" + (if (this == 1) "" else "s")

fun failureSummary(state: State, res: TestResult): String =
    failureSummaryAndReason(state, res).a

fun failureReason(state: State, res: TestResult): List<String> =
    failureSummaryAndReason(state, res).b

fun failureSummaryAndReason(state: State, res: TestResult): Tuple2<String, List<String>> {
    val header = if (res.expected) "*** Failed! " else "+++ OK, failed as expected. "

    fun count(full: Boolean): String = "(after ${(state.numSuccessTests + 1).number("test")}" +
            (
                    if (state.numSuccessShrinks > 0 || (full && state.numTryShrinks > 0))
                        " and ${state.numSuccessShrinks}" +
                                (if (full && state.numTryShrinks > 0) ".${state.numTryShrinks}" else "") +
                                " shrink" + (if (state.numSuccessShrinks == 1 && (full && state.numTryShrinks > 0).not()) "" else "s")
                    else "") + ")"

    val summary = header +
            res.reason.oneLine().short(26) + " " +
            count(true) + "..."

    val full = listOf(
        header +
                (if (res.reason.contains("\n")) "${res.reason} " else "") +
                count(false) + ":"
    ) + (if (res.reason.contains("\n")) emptyList() else res.reason.lines())

    return summary toT full
}

fun String.short(n: Int): String {
    val i = if (n >= 5) 3 else 0
    return if (n < length) take(n - 2 - i) + ".." + drop(length - i)
    else this
}

fun String.oneLine(): String = split("\n").joinToString(" ")

fun putSuccessStr(state: State): IO<Unit> {
    val res = labelsAndTables(state)
    val (short, long) = when {
        res.a.size == 1 -> listOf(" ( ${res.a.first().dropWhile { it.isWhitespace() }}).") toT res.b
        res.a.isEmpty() -> listOf(".") toT res.b
        else -> (listOf(":") + res.a) toT res.b
    }
    return state.output.update {
        it + listOf(short, long).filter { it.isNotEmpty() }.joinToString("") { it.joinToString("\n") } + "\n"
    }.fix()
}

fun labelsAndTables(state: State): Tuple2<List<String>, List<String>> {
    val numberedLabels = state.labels.toList().mapIndexed { index, pair ->
        index to mapOf(pair.first to pair.second).k()
    }.foldRight(emptyMap<Int, MapK<String, Int>>().k()) { v, acc ->
        mapOf(v).k().plus<Int, MapK<String, Int>>(MapK.semigroup(Int.semigroup()), acc)
    }

    val labels = ((if (state.classes.isEmpty()) emptyList() else listOf(state.classes)) + numberedLabels.values).map {
        showTable(state.numSuccessTests, none(), it)
    }.filter { it.isNotEmpty() }.map { it.joinToString("") }

    val tables = state.tables.toList().map {
        showTable(it.second.combineAll(Int.monoid()), it.first.some(), it.second)
    }.filter { it.isNotEmpty() }.map { it.joinToString("") }

    return labels toT tables
}

fun showTable(k: Int, tableName: Option<String>, table: Map<String, Int>): List<String> =
        listOf(tableName.fold({ "" }, { "$it (${"%.d".format(k)} in total)" })) +
                table.entries.sortedBy { it.key }.reversed().sortedBy { it.value }
                    .reversed().map { it.value.toPercentage(k) + "% ${it.key}" }
