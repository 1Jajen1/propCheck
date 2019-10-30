package propCheck

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.fx
import arrow.core.extensions.id.traverse.traverse
import arrow.core.extensions.list.foldable.sequence_
import arrow.core.extensions.list.traverse.traverse
import arrow.core.extensions.listk.monoid.monoid
import arrow.core.extensions.listk.semigroup.semigroup
import arrow.core.extensions.mapk.foldable.combineAll
import arrow.core.extensions.mapk.semigroup.plus
import arrow.core.extensions.mapk.semigroup.semigroup
import arrow.core.extensions.monoid
import arrow.core.extensions.semigroup
import arrow.core.extensions.sequence.foldable.foldRight
import arrow.core.extensions.sequence.foldable.isEmpty
import arrow.core.extensions.sequencek.monoid.monoid
import arrow.core.extensions.sequencek.semigroup.semigroup
import arrow.extension
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
import arrow.recursion.CoalgebraM
import arrow.typeclasses.Monad
import arrow.typeclasses.MonadSyntax
import arrow.typeclasses.Traverse
import org.apache.commons.math3.special.Erf
import propCheck.arbitrary.Gen
import propCheck.arbitrary.RandSeed
import propCheck.arbitrary.fix
import propCheck.arbitrary.gen.monad.monad
import propCheck.testresult.testable.testable
import kotlin.random.Random

/**
 * Running propcheck:
 * initializing state: IO because of generating a randomseed
 * runATest: R access to state
 *  ^- doneTesting: RW access to state
 *  ^- giveUpTesting: RW access to state
 *  ^- runATest: IO to unpack ioRose, IO for callbacks, RW access to state, IO access because of runATest
 *      ^- foundFailure
 *          ^- localMin: R access to state
 *              ^- _localMin: IO to unpack ioRose, IO for callbacks, RW access to state
 *                  ^- localMinFound: RW access to state, IO for callbacks
 * Everything else builds upon reduceRose to unpack the rose tree:
 * Unpacking Property:
 * Property
 *  -> Gen<Prop> -> RandSeed + Size -> Prop
 *      -> Rose<TestResult> -> reduceRose (flatten out io roses) ->
 *          -> IO<MkRose<TestResult>> -> runIO (^-^)
 *              -> MkRose<TestResult> // testresult + a lazy sequence of shrunk results
 *
 * That is, surprisingly, all the magic behind quickcheck. Which is damn simple!
 *
 * Changes:
 * - Move IORose out of Rose and make all it's creators return and handle m Rose instead
 *  - also make rose a base functor version and have the default tree contain nodes of type m Rose
 *  - Or go the hedgehog route of TreeT and NodeT, see what works better...
 *      - For now Rose<M, A> seems fine
 * - Go for a monadic stack like hedgehog (ignore monadic generators for now, but everything else needs a good stack
 *  - Error handling at the top and a writer for output. That should already help quite a bit.
 *      - When bifunctor io is out switch to that for error handling
 * - Pretty printer for output diffs
 *  - This needs a custom pretty printer (which can and will be another library) (Might also be useful for arrow-meta guys to test and print code)
 *  - Example: Diff User(y = 1, x = 10) User (y = 1, x = 11) => User(
 *                                                                 y = 1,
 *                                                              -  x = 10,
 *                                                              +  x = 11,
 *                                                              )
 * All of the above changes will mean no changes to the api, but enable a much better user experience.
 *  - These will have priority. Changing to integrated shrinking may come later.
 *
 * - Ranges instead of size? shrinking will invalidate this...
 *
 * Hedgehog:
 * Property => PropertyT IO () -> TestT (GenT IO) () -> ExceptT err (WriterT out (GenT IO)) ()
 *              -> ExceptT err (WriterT out (Seed -> Size -> TreeT (MaybeT IO))) ()
 * This can be summed up as:
 *  Error handling using ExceptT -> Output using WriterT -> Generating a TreeT (MaybeT IO)
 *  This is very similar to quickcheck except that quickcheck is basically only IO Gen ()
 *
 * Hedgehog's integrated shrinking works by letting GenT generate shrunk values alongside it and providing very smart means
 *  of mapping and flatMapping to shrink the new values with it. This has the obvious problem of not always shrinking the earlier parts of a monadic chain...
 */

// TODO remove when https://github.com/arrow-kt/arrow/pull/1750 is merged and released
/**
 * Monadic version of coelgot
 */
fun <F, M, A, B> A.coelgotM(
    f: (Tuple2<A, Eval<Kind<M, Kind<F, B>>>>) -> Kind<M, B>,
    coalg: CoalgebraM<F, M, A>,
    TF: Traverse<F>,
    MM: Monad<M>
): Kind<M, B> {
    fun h(a: A): Kind<M, B> =
        TF.run {
            MM.run {
                f(
                    Tuple2(a, Eval.later { coalg(a).flatMap { it.map(::h).sequence(MM) } })
                )
            }
        }

    return h(this)
}

/**
 * Monadic version of elgot
 */
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
// TODO remove the above when the pr gets merged

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
    val abortWith: Option<Result>,
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
    val randomSeed: RandSeed,
    val requiredCoverage: MapK<Tuple2<Option<String>, String>, Double>
) {
    companion object
}

/**
 * Confidence used with checkCoverage to determine wether or not a test is sufficiently covered
 */
data class Confidence(
    val certainty: Long = Math.pow(10.0, 9.0).toLong(),
    val tolerance: Double = 0.9
)

data class Property(val unProperty: Gen<Prop>) {
    companion object
}

data class Prop(val unProp: Rose<ForIO, TestResult>)

/**
 * Base interface for testable data
 * Implemented by Boolean, TestResult, Property, Gen and Function1
 */
interface Testable<A> {
    fun A.property(): Property
}

interface BooleanTestable : Testable<Boolean> {
    override fun Boolean.property(): Property = TestResult.testable().run {
        liftBoolean(this@property).property()
    }
}

fun Boolean.property(): Property = TestResult.testable().run {
    liftBoolean(this@property).property()
}

fun Boolean.Companion.testable(): Testable<Boolean> = object : BooleanTestable {}

@extension
interface TestResultTestable : Testable<TestResult> {
    override fun TestResult.property(): Property =
        Property(
            Gen.monad().just(
                Prop(
                    protectResults(
                        Rose.just(
                            IO.monad(),
                            this
                        )
                    )
                )
            ).fix()
        )
}

@extension
interface PropertyTestable : Testable<Property> {
    override fun Property.property(): Property = Property(
        Gen.monad().fx.monad {
            this@property.unProperty.bind()
        }.fix()
    )
}

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

internal fun usedSeed(seed: RandSeed, size: Int): String =
    "\nused seed ${seed.seed}" + (if (seed.gamma != RandSeed.GOLDEN_GAMMA) " with gamma: " + seed.gamma else "") + " and with size $size"

internal fun failureMessage(result: Result): String = when (result) {
    is Result.Failure -> "Failed: " + result.reason +
            (if (result.failingTestCase.isNotEmpty())
                "\n" + result.failingTestCase.joinToString() + "\n"
            else "") +
            "after " + result.numTests.number("test") +
            (if (result.numDiscardedTests > 0)
                " discarded " + result.numDiscardedTests
            else "") +
            (if (result.numShrinks > 0)
                " shrunk " + result.numShrinks.number("time")
            else "") + usedSeed(result.usedSeed, result.usedSize)
    is Result.GivenUp -> "Gave up " + "after " + result.numTests.number("test") +
            (if (result.numDiscardedTests > 0)
                " discarded " + result.numDiscardedTests
            else "")
    is Result.NoExpectedFailure -> "No expected failure " + "after " + result.numTests.number("test") +
            (if (result.numDiscardedTests > 0)
                " discarded " + result.numDiscardedTests
            else "")
    else -> throw IllegalStateException("Never")
}

typealias Log = ListK<LogEntry>

// Soon to be expanded with richer debug options
data class LogEntry(val text: String)
// TODO make actual newtypes later on?
typealias PropCheck<A> = PropCheckT<ForIO, A>

typealias PropCheckT<M, A> = WriterT<M, Log, A>

// Helpers to circumvent the terrible type inference
fun writeLogEntry(l: LogEntry): PropCheck<Unit> = WriterT.tell(IO.applicative(), listOf(l).k())

fun writeText(t: String): PropCheck<Unit> = writeLogEntry(LogEntry(t))

fun <A> liftIO(io: IO<A>): PropCheck<A> = WriterT.liftF(io, ListK.monoid(), IO.applicative())
fun <A> propCheckFx(f: suspend MonadSyntax<WriterTPartialOf<ForIO, Log>>.() -> A): PropCheck<A> =
    WriterT.fx(IO.monad(), ListK.monoid(), f)

fun propCheckMonad(): Monad<WriterTPartialOf<ForIO, Log>> = WriterT.monad(IO.monad(), ListK.monoid())

fun renderLog(l: Log): String =
    l.foldRight(Eval.now("")) { v, acc ->
        Eval.fx {
            val prev = !acc
            // newlines everywhere except for the last entry
            if (prev != "") prev + "\n" + v.text
            else prev + v.text
        }
    }.value()

fun runTest(args: Args, prop: Property): PropCheck<Result> = propCheckFx {
    val initState = !liftIO(getInitialState(args))
    // TODO: Split this into pieces
    // elgotM with Id is isomorphic to elgotM with NonEmptyListF and an algebra that just returns the last element
    !initState.elgotM({ liftIO(IO.just(it.value())) }, { state ->
        // check current state if its fine
        if (state.numSuccessTests >= state.maxSuccess && state.coverageConfidence.isEmpty())
            doneTesting(state).map { it.left() }
        else if (state.numDiscardedTests >= state.maxDiscardRatio * Math.max(state.numSuccessTests, state.maxSuccess))
            giveUpTesting(state).map { it.left() }
        else state.abortWith.fold({
            // take a seed state and generate a new state (by running a test)
            runATest(state, prop).map(IO.functor()) { newState ->
                Id(newState).right()
            }
        }, { liftIO(IO.just(it.left())) }) // abort with a result
    }, Id.traverse(), propCheckMonad())
}.fix()

fun getInitialState(args: Args): IO<State> = IO.fx {
    val randSeed = args.replay.fold({
        IO { RandSeed(Random.nextLong()) }
    }, { IO.just(it.a) }).bind()

    fun roundTo(n: Int, m: Int): Int = (n / m) * m

    /**
     * Special size function to later calculate different input sizes for the generator
     */
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

    State(
        abortWith = none(),
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

fun runATest(state: State, prop: Property): PropCheck<State> = propCheckFx {
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
    }.fold({ prop }, ::identity)

    val size = state.computeSize(state.numSuccessTests)(state.numRecentlyDiscardedTests)

    /**
     * run a test and unfold the rose structure
     */
    /**
     * run a test and unfold the rose structure
     */
    val (res0, ts) = liftIO(
        liftIO(
            protectRose(
                IO.just(
                    f_or_cov.unProperty.unGen(
                        rand1 toT size
                    ).unProp
                )
            )
        ).bind().runRose.fix()
    ).bind()

    val res = callbackPostTest(state, res0).bind()

    /**
     * compute a new state by adding the current result
     */
    /**
     * compute a new state by adding the current result
     */
    val newState = State(
        abortWith = none(),
        coverageConfidence = res.optionCheckCoverage.or(state.coverageConfidence),
        maxSuccess = res.optionNumOfTests.getOrElse { state.maxSuccess },
        tables = res.tables.foldRight(state.tables) { (k, l), acc ->
            acc.k()
                .plus<String, MapK<String, Int>>(MapK.semigroup(Int.semigroup()), mapOf(k to (mapOf(l to 1)).k()).k())
        },
        expected = res.expected,
        labels = mapOf(*res.labels.map { it to 1 }.toTypedArray()).k().plus(Int.semigroup(), state.labels),
        classes = mapOf(*res.classes.map { it to 1 }.toTypedArray()).k().plus(Int.semigroup(), state.classes),
        requiredCoverage = res.requiredCoverage.foldRight(state.requiredCoverage) { v, acc ->
            val (key, value, p) = v
            val alreadyThere = acc[key toT value] ?: Double.MIN_VALUE
            (acc + mapOf((key toT value) to Math.max(alreadyThere, p))).k()
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

    fun cont(nState: State, br: (State) -> PropCheck<Result>): PropCheck<State> = if (res.abort)
        br(nState).map(IO.functor()) { State.abortWith.set(nState, it) }
    else
        liftIO(IO.just(nState))

    when (res.ok) {
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
                val res = if (nRes.expected.not())
                    Result.Success(
                        numTests = newState.numSuccessTests + 1,
                        numDiscardedTests = newState.numDiscardedTests,
                        tables = newState.tables,
                        labels = newState.labels,
                        classes = newState.classes
                    )
                else
                    Result.Failure(
                        usedSeed = newState.randomSeed,
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
                State.abortWith.set(newState, res)
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
    }.bind()
}

/**
 * Done testing, construct result and add some output
 */
fun doneTesting(state: State): PropCheck<Result> =
    if (state.expected) {
        propCheckFx {
            !writeText("+++ OK, passed ${showTestCount(state)}")
            !writeText(getSuccessStr(state))

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
            !writeText("*** Failed! Passed ${showTestCount(state)} (expected Failure)")
            !writeText(getSuccessStr(state))

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
    !writeText("*** Gave up! Passed only ${showTestCount(state)}")
    !writeText(getSuccessStr(state))

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
 *
 * TODO: Split this into pieces
 */
fun foundFailure(state: State, res: TestResult, ts: Sequence<Rose<ForIO, TestResult>>): FailureResult =
    (Tuple3(state, res, ts))
        .elgotM({ liftIO(IO.just(it.fix().value())) }, { (currState, currRes, remainingShrunk) ->
            if (currState.numSuccessShrinks + currState.numTotTryShrinks >= currState.maxShrinks || remainingShrunk.isEmpty())
                localMinFound(currState, currRes).map(IO.functor()) { it.left() }
            else propCheckFx {
                val (nRes0, nShrunk) = !liftIO(remainingShrunk.first().runRose.fix())
                val nRes = !callbackPostTest(currState, nRes0)
                if (nRes.ok == false.some())
                    Id(
                        Tuple3(
                            State.numTryShrinks.set(
                                State.numSuccessShrinks.modify(currState) { it + 1 }, 0
                            ),
                            nRes, nShrunk
                        )
                    ).right()
                else
                    Id(
                        Tuple3(
                            State.numTryShrinks.modify(
                                State.numTotTryShrinks.modify(currState) { it + 1 }
                            ) { it + 1 },
                            currRes,
                            remainingShrunk.drop(1)
                        )
                    ).right()
            }
        }, Id.traverse(), propCheckMonad()).fix()

/**
 * Done shrinking, append output. Back to runATest to finish up
 */
fun localMinFound(state: State, res: TestResult): FailureResult = propCheckFx {
    !failureReason(state, res).reversed().map { s ->
        writeText(s)
    }.sequence_(propCheckMonad())

    !callbackPostFinalFailure(state, res)

    Tuple4(state.numSuccessShrinks, state.numTotTryShrinks - state.numTryShrinks, state.numTryShrinks, res)
}

/**
 * call all post test callbacks
 */
fun callbackPostTest(state: State, res: TestResult): PropCheck<TestResult> =
    res.callbacks.filter { it is Callback.PostTest }.traverse(propCheckMonad()) {
        (it as Callback.PostTest).fn(state, res)
    }.fix().flatMap(IO.monad(), ListK.semigroup()) { liftIO(IO.just(res)) }

/**
 * call all post final failure callbacks
 */
fun callbackPostFinalFailure(state: State, res: TestResult): PropCheck<Unit> =
    res.callbacks.filter { it is Callback.PostFinalFailure }.traverse(propCheckMonad()) {
        (it as Callback.PostFinalFailure).fn(state, res)
    }.fix().unit(IO.functor())

// ----------------- Text utility functions for showing results
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

fun getSuccessStr(state: State): String {
    val res = labelsAndTables(state)
    val (short, long) = when {
        res.a.size == 1 -> listOf(" (${res.a.first().dropWhile { it.isWhitespace() }}).") toT res.b
        res.a.isEmpty() -> listOf(".") toT res.b
        else -> (listOf(":") + res.a) toT res.b
    }
    return listOf(short, long).filter { it.isNotEmpty() }.joinToString("\n") { it.joinToString("\n") }
}

fun labelsAndTables(state: State): Tuple2<List<String>, List<String>> {
    val numberedLabels = state.labels.toList().mapIndexed { index, pair ->
        index to mapOf(pair.first to pair.second).k()
    }.foldRight(emptyMap<Int, MapK<String, Int>>().k()) { v, acc ->
        mapOf(v).k().plus<Int, MapK<String, Int>>(MapK.semigroup(Int.semigroup()), acc)
    }

    val labels = ((if (state.classes.isEmpty()) emptyList() else listOf(state.classes)) + numberedLabels.values).map {
        showTable(state.numSuccessTests, none(), it)
    }.map { it.filter { it.isNotEmpty() } }.filter { it.isNotEmpty() }.map { it.joinToString(". ") }

    val tables = (state.tables.toList().map {
        showTable(it.second.combineAll(Int.monoid()), it.first.some(), it.second)
    } + (
            allCoverage(state).filter { (_, _, tot, n, p) ->
                insufficientlyCovered(
                    state.coverageConfidence.map { it.certainty },
                    tot, n, p
                )
            }.map { (optTable, label, tot, n, p) ->
                listOf(
                    optTable.fold({ "Only " }, { "Table \'$it\' had only " }) +
                            n.toPercentage(tot) + "% " + label + ", but expected " + (p * 100) + "%"
                )
            })
            ).filter { it.isNotEmpty() }.map { it.joinToString("\n") }

    return labels toT tables
}

fun showTable(k: Int, tableName: Option<String>, table: Map<String, Int>): List<String> =
    listOf(tableName.fold({ "" }, { "$it ($k in total)" })) +
            table.entries.sortedBy { it.key }.reversed().sortedBy { it.value }
                .reversed().map { it.value.toPercentage(k) + "% ${it.key}" }

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
    return (p + z * z / (2 * n) + z * Math.sqrt(p * (1 - p) / n + z * z / (4 * n * n))) / (1 + z * z / n)
}

// ---------------- The below functions invnormcdf and inorm are copied from data-number-erf
// I really do not understand them well enough to guarantee their correct behaviour :/
fun invnormcdf(d: Double): Double = when (d) {
    0.0 -> Double.NEGATIVE_INFINITY
    1.0 -> Double.POSITIVE_INFINITY
    else -> {
        val x = inorm(d)
        val e = 0.5 * Erf.erfc(-x / Math.sqrt(2.0)) - d
        val u = e * Math.sqrt(2 * Math.PI) * Math.exp(x * x / 2)
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
            val q = Math.sqrt(-2 * Math.log(d))
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
                        failed("Insufficient coverage")
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