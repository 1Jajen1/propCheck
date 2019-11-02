package propCheck

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.id.traverse.traverse
import arrow.core.extensions.list.traverse.traverse
import arrow.core.extensions.listk.monoid.monoid
import arrow.core.extensions.listk.semigroup.semigroup
import arrow.core.extensions.mapk.foldable.combineAll
import arrow.core.extensions.mapk.semigroup.plus
import arrow.core.extensions.mapk.semigroup.semigroup
import arrow.core.extensions.monoid
import arrow.core.extensions.semigroup
import arrow.core.extensions.sequence.foldable.isEmpty
import arrow.fx.ForIO
import arrow.fx.IO
import arrow.fx.extensions.fx
import arrow.fx.extensions.io.applicative.applicative
import arrow.fx.extensions.io.functor.functor
import arrow.fx.extensions.io.monad.monad
import arrow.fx.fix
import arrow.mtl.WriterT
import arrow.mtl.WriterTPartialOf
import arrow.mtl.extensions.fx
import arrow.mtl.extensions.writert.functor.unit
import arrow.mtl.extensions.writert.monad.monad
import arrow.mtl.fix
import arrow.optics.optics
import arrow.recursion.AlgebraM
import arrow.typeclasses.Monad
import arrow.typeclasses.MonadSyntax
import arrow.typeclasses.Traverse
import org.apache.commons.math3.special.Erf
import pretty.*
import propCheck.arbitrary.RandSeed
import propCheck.property.*
import propCheck.property.testresult.testable.testable
import kotlin.math.exp
import kotlin.math.ln
import kotlin.math.pow
import kotlin.math.sqrt
import kotlin.random.Random

// TODO remove when https://github.com/arrow-kt/arrow/pull/1750 is merged and released
fun <F, M, A, B> B.elgotM(
    alg: AlgebraM<F, M, A>,
    f: (B) -> Kind<M, Either<A, Kind<F, B>>>,
    TF: Traverse<F>,
    MM: Monad<M>
): Kind<M, A> {
    fun h(b: B): Kind<M, A> = MM.run {
        f(b).flatMap {
            it.fold(MM::just) { MM.run { TF.run { it.traverse(MM, ::h).flatMap(alg) } } }
        }
    }

    return h(this)
}

/**
 * Datatype which describes the result of a test
 */
sealed class Result {
    /**
     * The test was successfull
     */
    data class Success(
        val labels: MapK<String, Int>, // Test case labels
        val classes: MapK<String, Int>, // Test case classes
        val tables: MapK<String, MapK<String, Int>>, // Test case tables
        val numTests: Int, // Number of successful tests
        val numDiscardedTests: Int // Number of discarded tests
    ) : Result()

    /**
     * The test failed for and a failure was not expected
     * Will also trigger on coverage failures
     */
    data class Failure(
        val usedSeed: RandSeed, // random seed used
        val usedSize: Int, // size parameter used
        val numTests: Int, // Number of successful tests
        val numDiscardedTests: Int, // Number of discarded tests
        val numShrinks: Int, // Number of successful shrinks
        val numShrinkTries: Int, // Number of failed shrinks
        val numShrinkFinal: Int, // Number of failed shrinks after the last successful one
        val reason: String, // Reason the test failed
        val exception: Option<Throwable>, // Exception that the test threw
        val failingTestCase: List<String>, // failing test case
        val failingLabels: List<String>, // failing test cases labels
        val failingClasses: List<String> // failing test cases classes
    ) : Result()

    /**
     * A test succeeded but was expected to fail
     */
    data class NoExpectedFailure(
        val labels: MapK<String, Int>, // Test case labels
        val classes: MapK<String, Int>, // Test case classes
        val tables: MapK<String, MapK<String, Int>>, // Test case tables
        val numTests: Int, // Number of successful tests
        val numDiscardedTests: Int // Number of discarded tests
    ) : Result()

    /**
     * A test discared more data than discardRatio permitted
     */
    data class GivenUp(
        val numTests: Int, // Number of successful tests
        val numDiscardedTests: Int, // Number of discarded tests
        val classes: MapK<String, Int>, // Test case classes
        val labels: MapK<String, Int>, // Test case labels
        val tables: MapK<String, MapK<String, Int>> // Test case tables
    ) : Result()
}

/**
 * Arguments passed to the top-level function propCheck
 */
data class Args(
    val replay: Option<Tuple2<RandSeed, Int>> = none(), // optional seed and size to replay a test
    val maxSuccess: Int = 100, // Maximum number of attempts (may be ignored when checkCoverage is used)
    val maxDiscardRatio: Int = 10, // Maximum ratio at which tests may be discarded
    val maxSize: Int = 100, // Maximum size parameter passed to generators
    val verbose: Boolean = true, // verbose output
    val maxShrinks: Int = Int.MAX_VALUE // maximum number of shrink attempts performed. 0 for no shrinking
)

/**
 * Internal state used while running a test
 */
@optics
data class State(
    val maxSuccess: Int,
    val coverageConfidence: Option<Confidence>,
    val maxDiscardRatio: Int,
    val computeSize: (Int, Int) -> Int,
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
    val randomSeed: RandSeed,
    val requiredCoverage: MapK<Tuple2<Option<String>, String>, Double>
) {
    companion object
}

/**
 * Confidence used with checkCoverage to determine wether or not a test is sufficiently covered
 */
data class Confidence(
    val certainty: Long = 10.0.pow(9.0).toLong(),
    val tolerance: Double = 0.9
)

/**
 * run property test on a given property and print the output
 */
fun propCheckIO(
    args: Args = Args(),
    f: () -> Property
): IO<Result> = IO.fx {
    val (logs, res) = !runTest(args, f()).value()
    !IO { println(renderLog(logs)) }
    res
}

/**
 * same as propCheckIO but throws an AssertionError at the end (useful for actually running inside test runners)
 */
fun propCheckIOWithError(
    args: Args = Args(),
    f: () -> Property
): IO<Unit> = propCheckIO(args, f).flatMap {
    when (it) {
        is Result.Success -> IO.unit
        is Result.GivenUp -> IO.raiseError(
            createAssertionError(it)
        )
        is Result.NoExpectedFailure -> IO.raiseError(
            createAssertionError(it)
        )
        is Result.Failure -> IO.raiseError(
            createAssertionError(it)
        )
    }
}

internal fun createAssertionError(res: Result): AssertionError = AssertionError(failureMessage(res)).apply {
    initCause(null)
}

internal fun usedSeed(seed: RandSeed, size: Int): Doc<Nothing> =
    "Used seed".text<Nothing>() spaced
            seed.seed.doc<Nothing>() +
            (if (seed.gamma != RandSeed.GOLDEN_GAMMA) space<Nothing>() + "with gamma".text() spaced seed.gamma.doc() else nil()) spaced
            "and with size".text() spaced
            size.doc()

internal fun failureMessage(result: Result): String = when (result) {
    is Result.Failure -> "Failed:".text<Nothing>() line result.reason.doc<Nothing>() +
            (if (result.failingTestCase.isNotEmpty())
                result.failingTestCase
                    .map { it.doc<Nothing>() }
                    .foldDoc { a, b -> a + hardLine() + b }
                    .enclose(hardLine(), hardLine())
            else nil()) +
            "after".text<Nothing>() spaced result.numTests.number<Nothing>("test".text(), "s".text()) +
            (if (result.numDiscardedTests > 0)
                dot<Nothing>() + space() + "Discarded".text() spaced result.numDiscardedTests.doc()
            else nil()) +
            (if (result.numShrinks > 0)
                dot<Nothing>() + space() + "Shrunk".text() spaced result.numShrinks.number("time".text(), "s".text())
            else nil()) + dot() spaced usedSeed(result.usedSeed, result.usedSize)
    is Result.GivenUp -> "Gave up".text<Nothing>() spaced "after".text() spaced result.numTests.number<Nothing>(
        "test".text(),
        "s".text()
    ) +
            (if (result.numDiscardedTests > 0)
                dot<Nothing>() + space() + "Discarded".text() spaced result.numDiscardedTests.doc()
            else dot())
    is Result.NoExpectedFailure -> "No expected failure".text<Nothing>() spaced "after".text() spaced result.numTests.number<Nothing>(
        "test".text(),
        "s".text()
    ) +
            (if (result.numDiscardedTests > 0)
                dot<Nothing>() + space() + "Discarded".text() spaced result.numDiscardedTests.doc()
            else dot())
    else -> throw IllegalStateException("Never")
}.renderPretty().renderString()

typealias Log = ListK<LogEntry>

// Soon to be expanded with richer debug options
data class LogEntry(val doc: Doc<Nothing>)

typealias PropCheck<A> = PropCheckT<ForIO, A>

typealias PropCheckT<M, A> = WriterT<M, Log, A>

// Helpers to circumvent the terrible type inference
fun writeLogEntry(l: LogEntry): PropCheck<Unit> = WriterT.tell(IO.applicative(), listOf(l).k())

@Deprecated("Use writeDoc instead", ReplaceWith("writeDoc(t.doc())", "propCheck.LogEntry", "pretty.doc"))
fun writeText(t: String): PropCheck<Unit> = writeLogEntry(LogEntry(t.doc()))

fun writeDoc(t: Doc<Nothing>): PropCheck<Unit> = writeLogEntry(LogEntry(t))

fun <A> liftIO(io: IO<A>): PropCheck<A> = WriterT.liftF(io, ListK.monoid(), IO.applicative())
fun <A> propCheckFx(f: suspend MonadSyntax<WriterTPartialOf<ForIO, Log>>.() -> A): PropCheck<A> =
    WriterT.fx(IO.monad(), ListK.monoid(), f)

fun propCheckMonad(): Monad<WriterTPartialOf<ForIO, Log>> = WriterT.monad(IO.monad(), ListK.monoid())

fun renderLog(l: Log): String =
    l.map { it.doc }
        .reversed()
        .foldDoc { a, b -> a + hardLine() + b }
        .renderPretty()
        .renderString()

fun runTest(args: Args, prop: Property): PropCheck<Result> = propCheckFx {
    val initState = !liftIO(getInitialState(args))
    // elgotM with Id is isomorphic to elgotM with NonEmptyListF and an algebra that just returns the last element
    !initState.elgotM({ liftIO(IO.just(it.value())) }, { state ->
        // check current state if its fine
        if (state.numSuccessTests >= state.maxSuccess && state.coverageConfidence.isEmpty())
            doneTesting(state).map { it.left() }
        else if (state.numDiscardedTests >= state.maxDiscardRatio * state.numSuccessTests.coerceAtLeast(state.maxSuccess))
            giveUpTesting(state).map { it.left() }
        else runATest(state, prop).map(IO.functor()) { it.map(::Id) }
    }, Id.traverse(), propCheckMonad())
}.fix()

fun getInitialState(args: Args): IO<State> = IO.fx {
    val randSeed = !args.replay.fold({
        IO { RandSeed(Random.nextLong()) }
    }, { IO.just(it.a) })

    fun roundTo(n: Int, m: Int): Int = (n / m) * m

    /**
     * Special size function to later calculate different input sizes for the generator
     */
    val computeSizeA: (Int, Int) -> Int = { n, d ->
        if (
            roundTo(n, args.maxSize) + args.maxSize <= args.maxSuccess ||
            n >= args.maxSuccess || args.maxSuccess.rem(args.maxSize) == 0
        )
            (n.rem(args.maxSize) + d / 10)
                .coerceAtMost(args.maxSize)
        else
            (n.rem(args.maxSize) * args.maxSize / (args.maxSuccess.rem(args.maxSize)) + d / 10)
                .coerceAtMost(args.maxSize)
    }

    fun at0(f: (Int, Int) -> Int, s: Int): (Int, Int) -> Int = { n, d ->
        when (n toT d) {
            Tuple2(0, 0) -> s
            else -> f(n, d)
        }
    }

    State(
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
        requiredCoverage = emptyMap<Tuple2<Option<String>, String>, Double>().k()
    )
}

fun runATest(state: State, prop: Property): PropCheck<Either<Result, State>> = propCheckFx {
    val (rand1, rand2) = state.randomSeed.split()

    /**
     * add a coverage check if needed
     */
    /**
     * add a coverage check if needed
     */
    val f_or_cov: Property = state.coverageConfidence.map { conf ->
        if (
            (1 + state.numSuccessTests).rem(100) == 0 &&
            ((1 + state.numSuccessTests) / 100).rem(2) == 0
        ) addCoverageCheck(conf, state, prop)
        else prop
    }.getOrElse { prop }

    val size = state.computeSize(state.numSuccessTests, state.numRecentlyDiscardedTests)

    /**
     * run a test and unfold the rose structure
     */
    /**
     * run a test and unfold the rose structure
     */
    val (res0, ts) = !liftIO(
        liftIO(
            protectRose(
                IO.just(
                    f_or_cov.unProperty.unGen(
                        rand1 toT size
                    ).unProp
                )
            )
        ).bind()
            .runRose.fix()
    )

    val res = !callbackPostTest(state, res0)

    /**
     * compute a new state by adding the current result
     */
    /**
     * compute a new state by adding the current result
     */
    val newState = State(
        coverageConfidence = res.optionCheckCoverage.or(state.coverageConfidence),
        maxSuccess = res.optionNumOfTests.getOrElse { state.maxSuccess },
        tables = res.tables.foldRight(state.tables) { (k, l), acc ->
            acc.k()
                .plus<String, MapK<String, Int>>(
                    MapK.semigroup(Int.semigroup()),
                    mapOf(k to (mapOf(l to 1)).k()).k()
                )
        },
        expected = res.expected,
        labels = mapOf(*res.labels.map { it to 1 }
            .toTypedArray()).k()
            .plus(Int.semigroup(), state.labels),
        classes = mapOf(*res.classes.map { it to 1 }
            .toTypedArray()).k()
            .plus(Int.semigroup(), state.classes),
        requiredCoverage = res.requiredCoverage
            .foldRight(state.requiredCoverage) { (key, value, p), acc ->
                val alreadyThere = acc[key toT value] ?: Double.MIN_VALUE
                (acc + mapOf((key toT value) to alreadyThere.coerceAtLeast(p))).k()
            },
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
        randomSeed = state.randomSeed
    )

    fun cont(nState: State, br: (State) -> PropCheck<Result>): PropCheck<Either<Result, State>> = if (res.abort)
        br(nState).map(IO.functor()) { it.left() }
    else
        liftIO(IO.just(nState.right()))

    !when (res.ok) {
        true.some() -> {
            cont(
                State.numSuccessTests.modify(
                    State.numRecentlyDiscardedTests.set(
                        State.randomSeed.set(newState, rand2),
                        0
                    )
                ) { it + 1 },
                ::doneTesting
            )
        }
        false.some() -> {
            propCheckFx {
                val (numShrinks, totFailed, lastFailed, nRes) = !foundFailure(newState, res, ts)
                if (nRes.expected.not())
                    Result.Success(
                        numTests = newState.numSuccessTests + 1,
                        numDiscardedTests = newState.numDiscardedTests,
                        tables = newState.tables,
                        labels = newState.labels,
                        classes = newState.classes
                    ).left()
                else
                    Result.Failure(
                        usedSeed = newState.randomSeed,
                        numDiscardedTests = newState.numDiscardedTests,
                        numTests = newState.numSuccessTests + 1,
                        exception = nRes.exception,
                        reason = nRes.reason.renderPretty().renderString(),
                        failingClasses = nRes.classes,
                        failingLabels = nRes.labels,
                        failingTestCase = nRes.testCase,
                        numShrinkFinal = lastFailed,
                        numShrinks = numShrinks,
                        numShrinkTries = totFailed,
                        usedSize = size
                    ).left()
            }
        }
        None -> {
            cont(
                State.numDiscardedTests.modify(
                    State.numRecentlyDiscardedTests.modify(
                        State.randomSeed.set(newState, rand2)
                    ) { it + 1 }
                ) { it + 1 }, ::giveUpTesting
            )
        }
        else -> throw IllegalStateException("Not possible")
    }
}

/**
 * Done testing, construct result and add some output
 */
fun doneTesting(state: State): PropCheck<Result> =
    if (state.expected) {
        propCheckFx {
            !writeDoc("+++ OK, passed".text<Nothing>() spaced showTestCount(state))
            !writeDoc(getSuccessStr(state))

            Result.Success(
                numTests = state.numSuccessTests,
                numDiscardedTests = state.numDiscardedTests,
                classes = state.classes,
                labels = state.labels,
                tables = state.tables
            )
        }
    } else {
        propCheckFx {
            !writeDoc(
                "*** Failed! Passed".text<Nothing>() spaced
                        showTestCount(state) spaced
                        "expected Failure".text<Nothing>()
                            .enclose(lParen(), rParen())
            )
            !writeDoc(getSuccessStr(state))

            Result.NoExpectedFailure(
                numTests = state.numSuccessTests,
                numDiscardedTests = state.numDiscardedTests,
                classes = state.classes,
                labels = state.labels,
                tables = state.tables
            )
        }
    }

/**
 * Give up testing because of a high discard ratio. Add some output and construct result
 */
fun giveUpTesting(state: State): PropCheck<Result> = propCheckFx {
    !writeDoc("*** Gave up! Passed only".text<Nothing>() spaced showTestCount(state))
    !writeDoc(getSuccessStr(state))

    Result.GivenUp(
        numTests = state.numSuccessTests,
        numDiscardedTests = state.numDiscardedTests,
        classes = state.classes,
        labels = state.labels,
        tables = state.tables
    )
}

typealias FailureResult = PropCheck<Tuple4<Int, Int, Int, TestResult>>

/**
 * handle a failure, will try to shrink failure case
 */
fun foundFailure(state: State, res: TestResult, ts: Sequence<Rose<ForIO, TestResult>>): FailureResult =
    (Tuple3(state, res, ts))
        .elgotM({ liftIO(IO.just(it.fix().value())) }, { (currState, currRes, remainingShrunk) ->
            if (currState.numSuccessShrinks + currState.numTotTryShrinks >= currState.maxShrinks || remainingShrunk.isEmpty())
                localMinFound(currState, currRes).map(IO.functor()) { it.left() }
            else propCheckFx {
                val (nRes0, nShrunk) = !liftIO(remainingShrunk.first().runRose.fix())
                val nRes = !callbackPostTest(currState, nRes0)
                val newSeed = if (nRes.ok == false.some())
                    Tuple3(
                        State.numTryShrinks.set(
                            State.numSuccessShrinks.modify(currState) { it + 1 }, 0
                        ),
                        nRes, nShrunk
                    )
                else
                    Tuple3(
                        State.numTryShrinks.modify(
                            State.numTotTryShrinks.modify(currState) { it + 1 }
                        ) { it + 1 },
                        currRes,
                        remainingShrunk.drop(1)
                    )
                Id(newSeed).right()
            }
        }, Id.traverse(), propCheckMonad()).fix()

/**
 * Done shrinking, append output. Back to runATest to finish up
 */
fun localMinFound(state: State, res: TestResult): FailureResult = propCheckFx {
    !writeDoc(failureReason(state, res))

    !callbackPostFinalFailure(state, res)

    Tuple4(state.numSuccessShrinks, state.numTotTryShrinks - state.numTryShrinks, state.numTryShrinks, res)
}

/**
 * call all post test callbacks
 */
fun callbackPostTest(state: State, res: TestResult): PropCheck<TestResult> =
    res.callbacks.filterIsInstance<Callback.PostTest>().traverse(propCheckMonad()) {
        it.fn(state, res)
    }.fix().flatMap(IO.monad(), ListK.semigroup()) { liftIO(IO.just(res)) }

/**
 * call all post final failure callbacks
 */
fun callbackPostFinalFailure(state: State, res: TestResult): PropCheck<Unit> =
    res.callbacks.filterIsInstance<Callback.PostFinalFailure>().traverse(propCheckMonad()) {
        it.fn(state, res)
    }.fix().unit(IO.functor())

// ----------------- Text utility functions for showing results
fun <A> showTestCount(state: State): Doc<A> =
    state.numSuccessTests.number<A>("test".text(), "s".text()) +
            (if (state.numDiscardedTests > 0) "; ${state.numDiscardedTests} discarded".text() else nil())

fun <A> Int.number(doc: Doc<A>, add: Doc<A>): Doc<A> = when (this) {
    1 -> this.doc<A>() spaced doc
    else -> this.doc<A>() spaced doc + add
}


fun <A> failureReason(state: State, res: TestResult): Doc<A> {
    val header = if (res.expected) "*** Failed! ".text<A>() else "+++ OK, failed as expected. ".text()

    fun count(full: Boolean): Doc<A> =
        ("after".text<A>() spaced (state.numSuccessTests + 1).number<A>("test".text(), "s".text()) +
                (
                        if (state.numSuccessShrinks > 0 || (full && state.numTryShrinks > 0))
                            space<A>() + "and".text() spaced state.numSuccessShrinks.doc<A>() +
                                    (if (full && state.numTryShrinks > 0) dot<A>() spaced state.numTryShrinks.doc() else nil()) spaced
                                    "shrink".text<A>() + (if (state.numSuccessShrinks == 1 && (full && state.numTryShrinks > 0).not()) nil() else "s".text())
                        else nil())
                ).enclose(lParen(), rParen())

    // casting Nothing to A should be perfectly fine as a Doc<Nothing> cannot add any annotations
    val full: Doc<A> = (header + nil<A>().flatAlt(res.reason.flatten() as Doc<A>) +
            count(false) + colon() + (hardLine<A>() + res.reason as Doc<A>).flatAlt(nil()))
        .group()

    return full
}

fun getSuccessStr(state: State): Doc<Nothing> {
    val res = labelsAndTables(state)
    val short = when {
        res.a.size == 1 -> res.a.first().enclose(lParen(), rParen())
        res.a.isEmpty() -> dot()
        else -> res.a.vSep()
    }
    return listOf(short, res.b.vSep()).vSep()
}

fun labelsAndTables(state: State): Tuple2<List<Doc<Nothing>>, List<Doc<Nothing>>> {
    val numberedLabels = state.labels.toList().mapIndexed { index, pair ->
        index to mapOf(pair.first to pair.second).k()
    }.foldRight(emptyMap<Int, MapK<String, Int>>().k()) { v, acc ->
        mapOf(v).k().plus<Int, MapK<String, Int>>(MapK.semigroup(Int.semigroup()), acc)
    }

    val labels = ((if (state.classes.isEmpty()) emptyList() else listOf(state.classes)) + numberedLabels.values).map {
        showTable(state.numSuccessTests, none(), it)
            .punctuate(dot<Nothing>() + space())
            .hCat()
    }

    val tables = (state.tables.toList().map {
        showTable(it.second.combineAll(Int.monoid()), it.first.some(), it.second)
            .vSep()
    } + (
            allCoverage(state).filter { (_, _, tot, n, p) ->
                insufficientlyCovered(
                    state.coverageConfidence.map { it.certainty },
                    tot, n, p
                )
            }.map { (optTable, label, tot, n, p) ->
                listOf(
                    optTable.fold(
                        {
                            "Only".text<Nothing>() + space()
                        },
                        {
                            "Table".text<Nothing>() spaced it.doc<Nothing>().enclose(
                                sQuote(),
                                sQuote()
                            ) spaced "had only".text<Nothing>() + space()
                        }) +
                            n.toPercentage(tot).text() + "%".text() spaced label.doc<Nothing>() + comma() spaced "but expected".text() spaced (p * 100).doc<Nothing>() + "%".text()
                ).vSep()
            }))

    return labels toT tables
}

fun showTable(k: Int, tableName: Option<String>, table: Map<String, Int>): List<Doc<Nothing>> =
    listOf(
        tableName.fold(
            { nil<Nothing>() },
            { it.doc<Nothing>() spaced (k.doc<Nothing>() spaced "in total".text()).enclose(lParen(), rParen()) })
    ) +
            table.entries.sortedBy { it.key }.reversed().sortedBy { it.value }
                .reversed().map { it.value.toPercentage(k).doc<Nothing>() + "%".text() spaced it.key.text() }

fun Number.toPercentage(max: Number): String =
    "%.2f".format(100 * (this.toDouble() / max.toDouble()))

// ------------------------- Coverage check functions
fun sufficientlyCovered(confidence: Confidence, n: Int, k: Int, p: Double): Boolean =
    wilsonLow(k, n, 1.toDouble() / confidence.certainty) >= confidence.tolerance * p

fun insufficientlyCovered(err: Option<Long>, n: Int, k: Int, p: Double): Boolean = err.fold({
    k < p * n
}, {
    wilsonHigh(k, n, 1.toDouble() / it) < p
})

fun wilsonLow(k: Int, n: Int, a: Double): Double = wilson(k, n, invnormcdf(a / 2))

fun wilsonHigh(k: Int, n: Int, a: Double): Double = wilson(k, n, invnormcdf(1 - a / 2))

fun wilson(k: Int, n: Int, z: Double): Double {
    val p = k / n.toDouble()
    return (p + z * z / (2 * n) + z * sqrt(p * (1 - p) / n + z * z / (4 * n * n))) / (1 + z * z / n)
}

// ---------------- The below functions invnormcdf and inorm are copied from data-number-erf
// I really do not understand them well enough to guarantee their correct behaviour :/
// If anyone reading this knows a kotlin mpp library that does all of this please tell me!
fun invnormcdf(d: Double): Double = when (d) {
    0.0 -> Double.NEGATIVE_INFINITY
    1.0 -> Double.POSITIVE_INFINITY
    else -> {
        val x = inorm(d)
        val e = 0.5 * Erf.erfc(-x / sqrt(2.0)) - d
        val u = e * sqrt(2 * Math.PI) * exp(x * x / 2)
        x - u / (1 + x * u / 2)
    }
}

// Taken from data-number-erf which in turn took from
//  http://home.online.no/~pjacklam/notes/invnorm/
// Accurate to about 1e-9.
fun inorm(d: Double): Double {

    val dLow = 0.02425

    val a1 = -3.969683028665376e+01
    val a2 = 2.209460984245205e+02
    val a3 = -2.759285104469687e+02
    val a4 = 1.383577518672690e+02
    val a5 = -3.066479806614716e+01
    val a6 = 2.506628277459239e+00

    val b1 = -5.447609879822406e+01
    val b2 = 1.615858368580409e+02
    val b3 = -1.556989798598866e+02
    val b4 = 6.680131188771972e+01
    val b5 = -1.328068155288572e+01

    val c1 = -7.784894002430293e-03
    val c2 = -3.223964580411365e-01
    val c3 = -2.400758277161838e+00
    val c4 = -2.549732539343734e+00
    val c5 = 4.374664141464968e+00
    val c6 = 2.938163982698783e+00

    val d1 = 7.784695709041462e-03
    val d2 = 3.224671290700398e-01
    val d3 = 2.445134137142996e+00
    val d4 = 3.754408661907416e+00

    return when {
        d < 0 -> Double.NaN
        d == 0.0 -> Double.NEGATIVE_INFINITY
        d < dLow -> {
            val q = sqrt(-2 * ln(d))
            (((((c1 * q + c2) * q + c3) * q + c4) * q + c5) * q + c6) /
                    ((((d1 * q + d2) * q + d3) * q + d4) * q + 1)
        }
        d < 1 - dLow -> {
            val q = d - 0.05
            val r = q * q
            (((((a1 * r + a2) * r + a3) * r + a4) * r + a5) * r + a6) * q /
                    (((((b1 * r + b2) * r + b3) * r + b4) * r + b5) * r + 1)
        }
        d <= 1 -> 0 - inorm(1 - d)
        else -> Double.NaN
    }
}

fun addCoverageCheck(confidence: Confidence, state: State, prop: Property): Property =
    allCoverage(state).let { allCov ->
        when {
            allCov.map { (_, _, tot, n, p) ->
                sufficientlyCovered(
                    confidence,
                    tot, n, p
                )
            }.fold(true) { acc, v -> acc && v } -> once(prop)
            allCov.map { (_, _, tot, n, p) ->
                insufficientlyCovered(
                    confidence.certainty.some(),
                    tot, n, p
                )
            }.fold(false) { acc, v -> acc || v } -> {
                val (labels, tables) = labelsAndTables(state)
                listOf(labels, tables).filter { it.isNotEmpty() }.map { it.joinToString("") }
                    .foldRight(TestResult.testable().run {
                        failed("Insufficient coverage".text())
                            .property()
                    }) { v, acc ->
                        counterexample({ v }, acc)
                    }
            }
            else -> prop
        }
    }

fun allCoverage(state: State): List<Tuple5<Option<String>, String, Int, Int, Double>> =
    state.requiredCoverage.entries.map { (k, p) ->
        val (key, value) = k

        val combinedCounts: MapK<Option<String>, MapK<String, Int>> = (state.tables.mapKeys { it.key.some() } +
                mapOf(None to state.classes)).k()

        val n = combinedCounts[key]?.let { it[value] } ?: 0

        val totals = state.tables.map { it.values.sum() }

        val tot = key.fold({
            state.numSuccessTests
        }, {
            totals[it] ?: 0
        })

        Tuple5(key, value, tot, n, p)
    }