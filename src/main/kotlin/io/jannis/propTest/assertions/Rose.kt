package io.jannis.propTest.assertions

import arrow.Kind
import arrow.core.*
import arrow.data.extensions.list.traverse.traverse
import arrow.effects.IO
import arrow.effects.extensions.io.applicative.applicative
import arrow.effects.extensions.io.applicativeError.handleError
import arrow.effects.extensions.io.monad.monad
import arrow.effects.fix
import arrow.extension
import arrow.typeclasses.Applicative
import arrow.typeclasses.Functor
import arrow.typeclasses.Monad
import arrow.typeclasses.Show
import io.jannis.propTest.*
import io.jannis.propTest.assertions.property.testable.testable
import io.jannis.propTest.assertions.rose.monad.monad
import io.jannis.propTest.assertions.testresult.testable.testable
import io.jannis.propTest.gen.monad.monad

/**
 * Callbacks. Can perform io based on state and results
 */
sealed class Callback {
    class PostTest(val kind: CallbackKind, val fn: (State, TestResult) -> IO<Unit>) : Callback()
    class PostFinalFailure(val kind: CallbackKind, val fn: (State, TestResult) -> IO<Unit>) : Callback()
}

sealed class CallbackKind {
    object Counterexample : CallbackKind()
    object NoCounterexample : CallbackKind()
}

/**
 * A single tests result
 */
data class TestResult(
    val ok: Option<Boolean>, // Some(true) => success, Some(false) => failure and None => discarded
    val expected: Boolean, // expected outcome
    val reason: String, // failure reason
    val exception: Option<Throwable>, // thrown exception
    val abort: Boolean, // if true aborts testing hereafter
    val optionNumOfTests: Option<Int>,
    val optionCheckCoverage: Option<Confidence>,
    val labels: List<String>,
    val classes: List<String>,
    val tables: List<Tuple2<String, String>>,
    val requiredCoverage: List<Tuple3<Option<String>, String, Double>>,
    val testCase: List<String>,
    val callbacks: List<Callback>
) {
    companion object
}

// @higherkind boilerplate
class ForRose private constructor() {
    companion object
}
typealias RoseOf<A> = arrow.Kind<ForRose, A>
typealias RoseKindedJ<A> = io.kindedj.Hk<ForRose, A>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <A> RoseOf<A>.fix(): Rose<A> =
    this as Rose<A>

/**
 * Recursive data structure.
 * At every level keeps both the current tested value and (lazily) the shrunk values
 */
sealed class Rose<A> : RoseOf<A> {
    class MkRose<A>(val res: A, val shrunk: Sequence<Rose<A>>) : Rose<A>()
    class IORose<A>(val ioRose: IO<Rose<A>>) : Rose<A>()

    fun <B> map(f: (A) -> B): Rose<B> = when (this) {
        is IORose -> IORose(ioRose.map { it.map(f) })
        is MkRose -> MkRose(f(res), shrunk.map { it.map(f) })
    }

    fun <B> flatMap(f: (A) -> Rose<B>): Rose<B> = joinRose(map(f))

    companion object {
        fun <A> just(a: A): Rose<A> = MkRose(a, emptySequence())
    }
}

@extension
interface RoseFunctor : Functor<ForRose> {
    override fun <A, B> Kind<ForRose, A>.map(f: (A) -> B): Kind<ForRose, B> = fix().map(f)
}

@extension
interface RoseApplicative : Applicative<ForRose> {
    override fun <A, B> Kind<ForRose, A>.ap(ff: Kind<ForRose, (A) -> B>): Kind<ForRose, B> =
        fix().flatMap { a -> ff.map { it(a) }.fix() }

    override fun <A> just(a: A): Kind<ForRose, A> = Rose.just(a)
}

@extension
interface RoseMonad : Monad<ForRose> {
    override fun <A, B> Kind<ForRose, A>.flatMap(f: (A) -> Kind<ForRose, B>): Kind<ForRose, B> =
        fix().flatMap { f(it).fix() }

    override fun <A> just(a: A): Kind<ForRose, A> = Rose.just(a)

    override fun <A, B> tailRecM(a: A, f: (A) -> Kind<ForRose, Either<A, B>>): Kind<ForRose, B> {
        val r = f(a).fix()
        return r.flatMap { either ->
            either.fold({ tailRecM(it, f).fix() }, { Rose.just(it) })
        }
    }
}

/**
 * wrap an IO with results safely
 */
fun ioRose(rose: IO<Rose<TestResult>>): Rose<TestResult> = Rose.IORose(
    protectRose(rose)
)

/**
 * catch exceptions in io code and fail the test if necessary
 */
fun protectRose(rose: IO<Rose<TestResult>>): IO<Rose<TestResult>> = rose.handleError {
    Rose.just(
        failed(
            reason = "Exception",
            exception = it.some()
        )
    )
}

/**
 * apply a function on a roses content
 */
fun <A> onRose(rose: Rose<A>, f: (A, Sequence<Rose<A>>) -> Rose<A>): Rose<A> = when (rose) {
    is Rose.MkRose -> f(rose.res, rose.shrunk)
    is Rose.IORose -> Rose.IORose(rose.ioRose.map { onRose(it, f) })
}

/**
 * catch exceptions in io code and fail if necessary
 */
fun protectResult(io: IO<TestResult>): IO<TestResult> = io.handleError {
    failed(
        reason = "Exception",
        exception = it.some()
    )
}

/**
 * wrap internal roses with ioRose and install error handlers
 */
fun protectResults(rose: Rose<TestResult>): Rose<TestResult> = onRose(rose) { x, rs ->
    Rose.IORose(
        IO.monad().binding {
            val y = protectResult(IO.just(x)).bind()
            Rose.MkRose(y, rs.map(::protectResults))
        }.fix()
    )
}

/**
 * "Execute" a rose by reducing it as far as possible
 */
fun reduceRose(rose: Rose<TestResult>): IO<Rose<TestResult>> = when (rose) {
    is Rose.IORose -> rose.ioRose.flatMap(::reduceRose)
    is Rose.MkRose -> IO.just(rose)
}

/**
 * join two roses
 */
fun <A> joinRose(rose: Rose<Rose<A>>): Rose<A> = when (rose) {
    is Rose.IORose -> Rose.IORose(rose.ioRose.map(::joinRose))
    is Rose.MkRose -> when (rose.res) {
        is Rose.IORose -> Rose.IORose(
            rose.res.ioRose.flatMap {
                IO.just(
                    joinRose(Rose.MkRose(it, rose.shrunk))
                )
            }
        )
        is Rose.MkRose -> Rose.MkRose(
            rose.res.res,
            rose.shrunk.map(::joinRose) + rose.res.shrunk
        )
    }
}

fun succeeded(): TestResult = TestResult(
    ok = true.some(),
    expected = true,
    reason = "",
    classes = emptyList(),
    labels = emptyList(),
    abort = false,
    exception = none(),
    optionCheckCoverage = none(),
    optionNumOfTests = none(),
    requiredCoverage = emptyList(),
    testCase = emptyList(),
    tables = emptyList(),
    callbacks = emptyList()
)

fun failed(reason: String, exception: Option<Throwable> = none()): TestResult = TestResult(
    ok = false.some(),
    expected = true,
    reason = reason,
    classes = emptyList(),
    labels = emptyList(),
    abort = false,
    exception = exception,
    optionCheckCoverage = none(),
    optionNumOfTests = none(),
    requiredCoverage = emptyList(),
    testCase = emptyList(),
    tables = emptyList(),
    callbacks = emptyList()
)

fun rejected(): TestResult = TestResult(
    ok = none(),
    expected = true,
    reason = "",
    classes = emptyList(),
    labels = emptyList(),
    abort = false,
    exception = none(),
    optionCheckCoverage = none(),
    optionNumOfTests = none(),
    requiredCoverage = emptyList(),
    testCase = emptyList(),
    tables = emptyList(),
    callbacks = emptyList()
)

fun liftBoolean(bool: Boolean): TestResult = if (bool) succeeded() else failed(
    reason = "Falsifiable"
)

/**
 * Map over the test result of a property and protect the results with error handlers
 */
fun <A> Testable<A>.mapResult(f: (TestResult) -> TestResult): (A) -> Property =
    mapRoseResult { protectResults(it.map(f)) }

/**
 * Map over the test result of a property. f must be total
 */
fun <A> Testable<A>.mapTotalResult(f: (TestResult) -> TestResult): (A) -> Property =
    mapRoseResult { it.map(f) }

/**
 * Map over a rose containing the TestResult, f must be total
 */
fun <A> Testable<A>.mapRoseResult(f: (Rose<TestResult>) -> Rose<TestResult>): (A) -> Property =
    mapProp { Prop(f(it.unProp)) }

/**
 * map over a Prop (wrapper around rose). f must be total
 */
fun <A> Testable<A>.mapProp(f: (Prop) -> Prop): (A) -> Property = { a ->
    Property(a.property().unProperty.map(f))
}

/**
 * Map over the size of a generator used with the property. Calling Gen.scale(f)
 */
fun <A> Testable<A>.mapSize(f: (Int) -> Int): (A) -> Property = { a ->
    Property(a.property().unProperty.scale(f))
}

/**
 * enable shrinking in a property
 */
fun <A, B> Testable<A>.shrinking(shrink: (B) -> Sequence<B>, arg: B, pf: (B) -> A): Property {
    fun props(b: B, alreadyTested: Set<B>): Rose<Gen<Prop>> = Rose.MkRose(
        pf(b).property().unProperty,
        shrink(b).filter { alreadyTested.contains(it).not() }.let {
            it.mapIndexed { i, v -> props(v, setOf(b) + it.take(i).toSet() + alreadyTested) }
        }
    )
    return Property(props(arg, setOf(arg)).promote(Rose.monad()).map { Prop(joinRose(it.fix().map { it.unProp })) })
}

/**
 * disable shrinking in a property by just supplying empty sequences
 */
fun <A> Testable<A>.noShrinking(): (A) -> Property =
    mapRoseResult {
        onRose(it) { res, _ -> Rose.MkRose(res, emptySequence()) }
    }

/**
 * print out a counterexample on failure
 */
fun <A> Testable<A>.counterexample(s: String): (A) -> Property =
    Property.testable().mapTotalResult { res ->
        TestResult(
            testCase = listOf(s) + res.testCase,
            ok = res.ok,
            reason = res.reason,
            exception = res.exception,
            requiredCoverage = res.requiredCoverage,
            optionNumOfTests = res.optionNumOfTests,
            abort = res.abort,
            optionCheckCoverage = res.optionCheckCoverage,
            labels = res.labels,
            classes = res.classes,
            expected = res.expected,
            tables = res.tables,
            callbacks = res.callbacks
        )
    }.compose(callback(Callback.PostFinalFailure(CallbackKind.Counterexample) { st, _ ->
        st.output.update {
            it + s + "\n"
        }.fix()
    }))

/**
 * expect the property to fail
 */
fun <A> Testable<A>.expectFailure(): (A) -> Property =
    mapTotalResult { res ->
        TestResult(
            ok = res.ok,
            reason = res.reason,
            exception = res.exception,
            requiredCoverage = res.requiredCoverage,
            optionNumOfTests = res.optionNumOfTests,
            abort = res.abort,
            optionCheckCoverage = res.optionCheckCoverage,
            labels = res.labels,
            classes = res.classes,
            expected = false,
            testCase = res.testCase,
            tables = res.tables,
            callbacks = res.callbacks
        )
    }

/**
 * Abort after one execution
 */
fun <A> Testable<A>.once(): (A) -> Property =
    mapTotalResult { res ->
        TestResult(
            ok = res.ok,
            reason = res.reason,
            exception = res.exception,
            requiredCoverage = res.requiredCoverage,
            optionNumOfTests = res.optionNumOfTests,
            abort = true,
            optionCheckCoverage = res.optionCheckCoverage,
            labels = res.labels,
            classes = res.classes,
            expected = res.expected,
            testCase = res.testCase,
            tables = res.tables,
            callbacks = res.callbacks
        )
    }

/**
 * Continue to execute test if needed
 */
fun <A> Testable<A>.again(): (A) -> Property =
    mapTotalResult { res ->
        TestResult(
            ok = res.ok,
            reason = res.reason,
            exception = res.exception,
            requiredCoverage = res.requiredCoverage,
            optionNumOfTests = res.optionNumOfTests,
            abort = false,
            optionCheckCoverage = res.optionCheckCoverage,
            labels = res.labels,
            classes = res.classes,
            expected = res.expected,
            testCase = res.testCase,
            tables = res.tables,
            callbacks = res.callbacks
        )
    }

/**
 * Change the maximum tested amount
 */
fun <A> Testable<A>.withMaxSuccess(maxSuccess: Int): (A) -> Property =
    mapTotalResult { res ->
        TestResult(
            ok = res.ok,
            reason = res.reason,
            exception = res.exception,
            requiredCoverage = res.requiredCoverage,
            optionNumOfTests = maxSuccess.some(),
            abort = res.abort,
            optionCheckCoverage = res.optionCheckCoverage,
            labels = res.labels,
            classes = res.classes,
            expected = res.expected,
            testCase = res.testCase,
            tables = res.tables,
            callbacks = res.callbacks
        )
    }

/**
 * enable coverage checks
 * Without tests will not fail (only print warnings) when coverage is not satisfied. Using this will make tests fail
 */
fun <A> Testable<A>.checkCoverage(confidence: Confidence = Confidence()): (A) -> Property =
    mapTotalResult { res ->
        TestResult(
            ok = res.ok,
            reason = res.reason,
            exception = res.exception,
            requiredCoverage = res.requiredCoverage,
            optionNumOfTests = res.optionNumOfTests,
            abort = res.abort,
            optionCheckCoverage = confidence.some(),
            labels = res.labels,
            classes = res.classes,
            expected = res.expected,
            testCase = res.testCase,
            tables = res.tables,
            callbacks = res.callbacks
        )
    }

/**
 * Append a label to a test case
 */
fun <A> Testable<A>.label(label: String): (A) -> Property =
    mapTotalResult { res ->
        TestResult(
            ok = res.ok,
            reason = res.reason,
            exception = res.exception,
            requiredCoverage = res.requiredCoverage,
            optionNumOfTests = res.optionNumOfTests,
            abort = res.abort,
            optionCheckCoverage = res.optionCheckCoverage,
            labels = listOf(label) + res.labels,
            classes = res.classes,
            expected = res.expected,
            testCase = res.testCase,
            tables = res.tables,
            callbacks = res.callbacks
        )
    }

/**
 * implicitly label all cases
 */
fun <A, B> Testable<A>.collect(b: B, showB: Show<B> = Show.any()): (A) -> Property =
    label(showB.run { b.show() })

/**
 * classify tests based on some boolean value
 */
fun <A> Testable<A>.classify(bool: Boolean, label: String): (A) -> Property =
    if (!bool) { a: A -> a.property() }
    else mapTotalResult { res ->
        TestResult(
            ok = res.ok,
            reason = res.reason,
            exception = res.exception,
            requiredCoverage = res.requiredCoverage,
            optionNumOfTests = res.optionNumOfTests,
            abort = res.abort,
            optionCheckCoverage = res.optionCheckCoverage,
            labels = res.labels,
            classes = listOf(label) + res.classes,
            expected = res.expected,
            testCase = res.testCase,
            tables = res.tables,
            callbacks = res.callbacks
        )
    }

/**
 * add required coverage based on a boolean prop for a specific label
 */
fun <A> Testable<A>.cover(p: Double, bool: Boolean, label: String): (A) -> Property =
    Property.testable().mapTotalResult { res ->
        TestResult(
            ok = res.ok,
            reason = res.reason,
            exception = res.exception,
            requiredCoverage = listOf(Tuple3(none<String>(), label, p / 100)) + res.requiredCoverage,
            optionNumOfTests = res.optionNumOfTests,
            abort = res.abort,
            optionCheckCoverage = res.optionCheckCoverage,
            labels = res.labels,
            classes = res.classes,
            expected = res.expected,
            testCase = res.testCase,
            tables = res.tables,
            callbacks = res.callbacks
        )
    }.compose(classify(bool, label))

/**
 * tabulate data
 */
fun <A> Testable<A>.tabulate(key: String, values: List<String>): (A) -> Property =
    mapTotalResult { res ->
        TestResult(
            ok = res.ok,
            reason = res.reason,
            exception = res.exception,
            requiredCoverage = res.requiredCoverage,
            optionNumOfTests = res.optionNumOfTests,
            abort = res.abort,
            optionCheckCoverage = res.optionCheckCoverage,
            labels = res.labels,
            classes = res.classes,
            expected = res.expected,
            testCase = res.testCase,
            tables = values.map { key toT it } + res.tables,
            callbacks = res.callbacks
        )
    }

/**
 * add required coverage based on tables
 */
fun <A> Testable<A>.coverTable(key: String, values: List<Tuple2<String, Double>>): (A) -> Property =
    mapTotalResult { res ->
        TestResult(
            ok = res.ok,
            reason = res.reason,
            exception = res.exception,
            requiredCoverage = values.map { Tuple3(key.some(), it.a, it.b / 100) } + res.requiredCoverage,
            optionNumOfTests = res.optionNumOfTests,
            abort = res.abort,
            optionCheckCoverage = res.optionCheckCoverage,
            labels = res.labels,
            classes = res.classes,
            expected = res.expected,
            testCase = res.testCase,
            tables = res.tables,
            callbacks = res.callbacks
        )
    }

/**
 * append a callback to the property
 */
fun <A> Testable<A>.callback(cb: Callback): (A) -> Property =
    mapTotalResult { res ->
        TestResult(
            callbacks = res.callbacks + listOf(cb),
            ok = res.ok,
            reason = res.reason,
            exception = res.exception,
            requiredCoverage = res.requiredCoverage,
            optionNumOfTests = res.optionNumOfTests,
            abort = res.abort,
            optionCheckCoverage = res.optionCheckCoverage,
            labels = res.labels,
            classes = res.classes,
            expected = res.expected,
            testCase = res.testCase,
            tables = res.tables
        )
    }

/**
 * execute some code when a test fails
 */
fun <A> Testable<A>.whenFail(f: () -> Unit): (A) -> Property =
    whenFailIO(IO { f() })

/**
 * execute some io code when a test fails
 */
fun <A> Testable<A>.whenFailIO(f: IO<Unit>): (A) -> Property =
    callback(Callback.PostFinalFailure(CallbackKind.NoCounterexample) { _, _ -> f })

/**
 * execute some code on every failure (not just the last one)
 */
fun <A> Testable<A>.whenFailEvery(f: () -> Unit): (A) -> Property =
    whenFailEveryIO(IO { f() })

/**
 * execute some io code on every failure (not just the last one)
 */
fun <A> Testable<A>.whenFailEveryIO(f: IO<Unit>): (A) -> Property =
    callback(Callback.PostTest(CallbackKind.NoCounterexample) { _, res ->
        if (res.ok == false.some()) f
        else IO.unit
    })

/**
 * append a lot more output
 */
fun <A> Testable<A>.verbose(): (A) -> Property {
    fun status(res: TestResult): String = when (res.ok) {
        true.some() -> "Passed"
        false.some() -> "Failed"
        None -> "Skipped (precondition false)"
        else -> throw IllegalStateException("Not possible")
    }

    fun newCb(cbs: List<Callback>): Callback = Callback.PostTest(CallbackKind.Counterexample) { st, res ->
        IO.monad().binding {
            st.output.update {
                it + status(res) + ":" + "\n"
            }.bind()
            cbs.filter { it is Callback.PostFinalFailure && it.kind == CallbackKind.Counterexample }
                .traverse(IO.applicative()) {
                    (it as Callback.PostFinalFailure).fn(st, res)
                }.bind()
            st.output.update {
                it + "\n"
            }.bind()
        }.fix()
    }
    return mapResult { res ->
        TestResult(
            callbacks = listOf(newCb(res.callbacks)) + res.callbacks,
            ok = res.ok,
            reason = res.reason,
            exception = res.exception,
            requiredCoverage = res.requiredCoverage,
            optionNumOfTests = res.optionNumOfTests,
            abort = res.abort,
            optionCheckCoverage = res.optionCheckCoverage,
            labels = res.labels,
            classes = res.classes,
            expected = res.expected,
            testCase = res.testCase,
            tables = res.tables
        )
    }
}

/**
 * append shrinking output as well
 */
fun <A> Testable<A>.verboseShrinking(): (A) -> Property {
    fun newCb(cbs: List<Callback>): Callback = Callback.PostTest(CallbackKind.Counterexample) { st, res ->
        if (res.ok == false.some())
            IO.monad().binding {
                st.output.update {
                    it + "Failed:" + "\n"
                }.bind()
                cbs.filter { it is Callback.PostFinalFailure && it.kind == CallbackKind.Counterexample }
                    .traverse(IO.applicative()) {
                        (it as Callback.PostFinalFailure).fn(st, res)
                    }.bind()
                st.output.update {
                    it + "\n"
                }.bind()
            }.fix()
        else IO.unit
    }
    return mapResult { res ->
        TestResult(
            callbacks = listOf(newCb(res.callbacks)) + res.callbacks,
            ok = res.ok,
            reason = res.reason,
            exception = res.exception,
            requiredCoverage = res.requiredCoverage,
            optionNumOfTests = res.optionNumOfTests,
            abort = res.abort,
            optionCheckCoverage = res.optionCheckCoverage,
            labels = res.labels,
            classes = res.classes,
            expected = res.expected,
            testCase = res.testCase,
            tables = res.tables
        )
    }
}

/**
 * discared data based on boolean
 */
fun <A> Testable<A>.assert(bool: Boolean): (A) -> Property =
    if (bool) { a: A -> a.property() }
    else { a: A -> TestResult.testable().run { rejected().property() } }

/**
 * run a test with a given generator and without shrinking
 */
fun <A, B> Testable<A>.forAll(genB: Gen<B>, showB: Show<B> = Show.any()): ((B) -> A) -> Property =
    forAllShrink(genB, showB) { emptySequence() }

/**
 * run a test without printing a counterexample on failure
 */
fun <A, B> Testable<A>.forAllBlind(genB: Gen<B>): ((B) -> A) -> Property =
    forAllShrinkBlind(genB) { emptySequence() }

/**
 * run a test with shrinking
 */
fun <A, B> Testable<A>.forAllShrink(
    genB: Gen<B>,
    showB: Show<B> = Show.any(),
    shrinkerB: (B) -> Sequence<B>
): ((B) -> A) -> Property =
    forAllShrinkShow(genB, shrinkerB, { showB.run { it.show() } })

/**
 * run a test with shrinking and a specific show function
 */
fun <A, B> Testable<A>.forAllShrinkShow(
    genB: Gen<B>,
    shrinkerB: (B) -> Sequence<B>,
    showerB: (B) -> String
): ((B) -> A) -> Property = { pf ->
    Property.testable().forAllShrinkBlind(genB, shrinkerB).invoke { x ->
        counterexample(showerB(x)).invoke(pf(x))
    }
}

/**
 * run a test with shrinking but without showing a counterexample
 */
fun <A, B> Testable<A>.forAllShrinkBlind(
    genB: Gen<B>,
    shrinkerB: (B) -> Sequence<B>
): ((B) -> A) -> Property = { pf ->
    Property.testable().again().invoke(
        Property(
            Gen.monad().binding {
                shrinking(shrinkerB, genB.bind(), pf).unProperty.bind()
            }.fix()
        )
    )
}

/**
 * Helper that looks up instances based on generics
 */
inline fun <A, reified B: Any>Testable<A>.forAll(arbB: Arbitrary<B> = defArbitrary(), showB: Show<B> = defShow()): ((B) -> A) -> Property =
    forAll(arbB.arbitrary(), showB)

/**
 * Helper that looks up instances based on generics
 */
inline fun <A, reified B: Any>Testable<A>.forAllBlind(arbB: Arbitrary<B> = defArbitrary()): ((B) -> A) -> Property =
    forAllBlind(arbB.arbitrary())

/**
 * Helper that looks up instances based on generics
 */
inline fun <A, reified B: Any>Testable<A>.forAllShrink(arbB: Arbitrary<B> = defArbitrary(), showB: Show<B> = defShow()): ((B) -> A) -> Property =
        forAllShrink(arbB.arbitrary(), showB) { arbB.shrink(it) }

/**
 * Helper that looks up instances based on generics
 */
inline fun <A, reified B: Any>Testable<A>.forAllShrinkBlind(arbB: Arbitrary<B> = defArbitrary()): ((B) -> A) -> Property =
    forAllShrinkBlind(arbB.arbitrary()) { arbB.shrink(it) }

/**
 * test an io property (does not shrink)
 */
fun <A> Testable<A>.ioProperty(): (IO<A>) -> Property = { io: IO<A> ->
    Property.testable().idempotentIOProperty().invoke(
        io.map { noShrinking().invoke(it) }
    )
}

/**
 * test an io property with shrinking
 */
fun <A> Testable<A>.idempotentIOProperty(): (IO<A>) -> Property = { ioA: IO<A> ->
    Property(
        ioA.map {
            it.property().unProperty
        }.promote(IO.monad()).map { Prop(ioRose(it.fix().map { it.unProp })) }
    )
}