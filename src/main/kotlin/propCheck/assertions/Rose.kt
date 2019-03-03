package propCheck.assertions

import arrow.Kind
import arrow.core.*
import arrow.data.extensions.list.traverse.traverse
import arrow.effects.IO
import arrow.effects.extensions.io.applicative.applicative
import arrow.effects.extensions.io.applicativeError.handleError
import arrow.effects.extensions.io.monad.monad
import arrow.effects.fix
import arrow.extension
import arrow.optics.optics
import arrow.typeclasses.Applicative
import arrow.typeclasses.Functor
import arrow.typeclasses.Monad
import arrow.typeclasses.Show
import propCheck.*
import propCheck.assertions.property.testable.testable
import propCheck.assertions.rose.monad.monad
import propCheck.assertions.testresult.testable.testable
import propCheck.gen.monad.monad

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
@optics
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

internal fun defaultRes(): TestResult = TestResult(
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

fun succeeded(): TestResult = TestResult.ok.set(defaultRes(), true)

fun failed(reason: String, exception: Option<Throwable> = none()): TestResult =
    TestResult.optionException.set(
        TestResult.reason.set(
            TestResult.ok.set(defaultRes(), false), reason
        ), exception
    )

fun rejected(): TestResult = TestResult.optionOk.set(defaultRes(), none())

fun liftBoolean(bool: Boolean): TestResult = if (bool) succeeded() else failed(
    reason = "Falsifiable"
)

inline fun <reified A> defTestable(): Testable<A> = when (A::class.qualifiedName) {
    Property::class.qualifiedName -> Property.testable()
    TestResult::class.qualifiedName -> TestResult.testable()
    Boolean::class.qualifiedName -> Boolean.testable()
    else -> throw IllegalStateException("Could not find testable for ${A::class.qualifiedName}")
} as Testable<A>

/**
 * Map over the test result of a property and protect the results with error handlers
 */
inline fun <reified A> mapResult(
    noinline f: (TestResult) -> TestResult,
    a: A
): Property =
    mapRoseResult({ protectResults(it.map(f)) }, a)

/**
 * Map over the test result of a property. f must be total
 */
inline fun <reified A> mapTotalResult(
    noinline f: (TestResult) -> TestResult,
    a: A
): Property =
    mapRoseResult({ it.map(f) }, a)

/**
 * Map over a rose containing the TestResult, f must be total
 */
inline fun <reified A> mapRoseResult(
    noinline f: (Rose<TestResult>) -> Rose<TestResult>,
    a: A
): Property =
    mapProp({ Prop(f(it.unProp)) }, a)

/**
 * map over a Prop (wrapper around rose). f must be total
 */
inline fun <reified A> mapProp(noinline f: (Prop) -> Prop, a: A, testable: Testable<A> = defTestable()): Property =
    Property(testable.run { a.property() }.unProperty.map(f))

/**
 * Map over the size of a generator used with the property. Calling Gen.scale(f)
 */
inline fun <reified A> mapSize(noinline f: (Int) -> Int, a: A, testable: Testable<A> = defTestable()): Property =
    Property(testable.run { a.property() }.unProperty.scale(f))

/**
 * enable shrinking in a property
 */
inline fun <reified A, B> shrinking(
    testable: Testable<A> = defTestable(),
    noinline shrink: (B) -> Sequence<B>,
    arg: B,
    noinline pf: (B) -> A
): Property = Property(
    props(
        testable,
        shrink,
        pf,
        arg,
        setOf(arg)
    ).promote(Rose.monad()).map { Prop(joinRose(it.fix().map { it.unProp })) })

@PublishedApi
internal fun <A, B> props(
    testable: Testable<A>,
    shrink: (B) -> Sequence<B>,
    pf: (B) -> A,
    b: B,
    alreadyTested: Set<B>
): Rose<Gen<Prop>> = Rose.MkRose(
    testable.run { pf(b).property() }.unProperty,
    shrink(b).filter { alreadyTested.contains(it).not() }.let {
        it.mapIndexed { i, v -> props(testable, shrink, pf, v, setOf(b) + it.take(i).toSet() + alreadyTested) }
    }
)

/**
 * disable shrinking in a property by just supplying empty sequences
 */
inline fun <reified A> noShrinking(a: A): Property =
    mapRoseResult({
        onRose(it) { res, _ -> Rose.MkRose(res, emptySequence()) }
    }, a)

/**
 * print out a counterexample on failure
 */
inline fun <reified A> counterexample(s: String, a: A): Property =
    mapTotalResult({ res ->
        TestResult.testCase.modify(res) { listOf(s) + it }
    }, callback(Callback.PostFinalFailure(CallbackKind.Counterexample) { st, _ ->
        st.output.update {
            it + s + "\n"
        }.fix()
    }, a))

/**
 * expect the property to fail
 */
inline fun <reified A> expectFailure(a: A): Property =
    mapTotalResult({ res ->
        TestResult.expected.set(res, false)
    }, a)

/**
 * Abort after one execution
 */
inline fun <reified A> once(a: A): Property =
    mapTotalResult({ res ->
        TestResult.abort.set(res, true)
    }, a)

/**
 * Continue to execute test if needed
 */
inline fun <reified A> again(a: A): Property =
    mapTotalResult({ res ->
        TestResult.abort.set(res, false)
    }, a)

/**
 * Change the maximum tested amount
 */
inline fun <reified A> withMaxSuccess(maxSuccess: Int, a: A): Property =
    mapTotalResult({ res ->
        TestResult.optionNumOfTests.set(res, maxSuccess)
    }, a)

/**
 * enable coverage checks
 * Without tests will not fail (only print warnings) when coverage is not satisfied. Using this will make tests fail
 */
inline fun <reified A> checkCoverage(
    confidence: Confidence = Confidence(),
    a: A
): Property =
    mapTotalResult({ res ->
        TestResult.optionCheckCoverage.set(res, confidence)
    }, a)

/**
 * Append a label to a test case
 */
inline fun <reified A> label(label: String, a: A): Property =
    mapTotalResult({ res ->
        TestResult.labels.modify(res) { listOf(label) + it }
    }, a)

/**
 * implicitly label all cases
 */
inline fun <reified A, B> collect(
    b: B,
    a: A,
    showB: Show<B> = Show.any()
): Property =
    label(showB.run { b.show() }, a)

/**
 * classify tests based on some boolean value
 */
inline fun <reified A> classify(bool: Boolean, label: String, a: A, testable: Testable<A> = defTestable()): Property =
    if (!bool) testable.run { a.property() }
    else mapTotalResult({ res ->
        TestResult.classes.modify(res) { listOf(label) + it }
    }, a)

/**
 * add required coverage based on a boolean prop for a specific label
 */
inline fun <reified A> cover(
    p: Double,
    bool: Boolean,
    label: String,
    a: A
): Property =
    mapTotalResult({ res ->
        TestResult.requiredCoverage.modify(res) { listOf(Tuple3(none<String>(), label, p / 100)) + it }
    }, classify(bool, label, a))

/**
 * tabulate data
 */
inline fun <reified A> tabulate(
    key: String,
    values: List<String>,
    a: A
): Property =
    mapTotalResult({ res ->
        TestResult.tables.modify(res) { values.map { key toT it } + it }
    }, a)

/**
 * add required coverage based on tables
 */
inline fun <reified A> coverTable(
    key: String,
    values: List<Tuple2<String, Double>>,
    a: A
): Property =
    mapTotalResult({ res ->
        TestResult.requiredCoverage.modify(res) { values.map { Tuple3(key.some(), it.a, it.b / 100) } + it }
    }, a)

/**
 * append a callback to the property
 */
inline fun <reified A> callback(cb: Callback, a: A): Property =
    mapTotalResult({ res ->
        TestResult.callbacks.modify(res) { it + listOf(cb) }
    }, a)

/**
 * execute some code when a test fails
 */
inline fun <reified A> whenFail(noinline f: () -> Unit, a: A): Property =
    whenFailIO(IO { f() }, a)

/**
 * execute some io code when a test fails
 */
inline fun <reified A> whenFailIO(f: IO<Unit>, a: A): Property =
    callback(Callback.PostFinalFailure(CallbackKind.NoCounterexample) { _, _ -> f }, a)

/**
 * execute some code on every failure (not just the last one)
 */
inline fun <reified A> whenFailEvery(noinline f: () -> Unit, a: A): Property =
    whenFailEveryIO(IO { f() }, a)

/**
 * execute some io code on every failure (not just the last one)
 */
inline fun <reified A> whenFailEveryIO(f: IO<Unit>, a: A): Property =
    callback(Callback.PostTest(CallbackKind.NoCounterexample) { _, res ->
        if (res.ok == false.some()) f
        else IO.unit
    }, a)

/**
 * append a lot more output
 */
inline fun <reified A> verbose(a: A): Property =
    mapResult({ res ->
        TestResult.callbacks.modify(res) { listOf(newCb(it)) + it }
    }, a)

internal fun status(res: TestResult): String = when (res.ok) {
    true.some() -> "Passed"
    false.some() -> "Failed"
    None -> "Skipped (precondition false)"
    else -> throw IllegalStateException("Not possible")
}

@PublishedApi
internal fun newCb(cbs: List<Callback>): Callback = Callback.PostTest(CallbackKind.Counterexample) { st, res ->
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

/**
 * append shrinking output as well
 */
inline fun <reified A> verboseShrinking(a: A): Property =
    mapResult({ res ->
        TestResult.callbacks.modify(res) { listOf(newCb2(it)) + it }
    }, a)

@PublishedApi
internal fun newCb2(cbs: List<Callback>): Callback = Callback.PostTest(CallbackKind.Counterexample) { st, res ->
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

/**
 * discared data based on boolean
 */
inline fun <reified A> discardIf(bool: Boolean, a: A, testable: Testable<A> = defTestable()): Property =
    if (!bool) testable.run { a.property() }
    else TestResult.testable().run { rejected().property() }

/**
 * run a test with a given generator and without shrinking
 */
inline fun <reified A, B> forAll(
    genB: Gen<B>,
    testable: Testable<A> = defTestable(),
    showB: Show<B> = Show.any(),
    noinline prop: (B) -> A
): Property =
    forAllShrink(genB, showB, { emptySequence() }, testable, prop)

/**
 * run a test without printing a counterexample on failure
 */
inline fun <reified A, B> forAllBlind(
    genB: Gen<B>,
    testable: Testable<A> = defTestable(),
    noinline prop: (B) -> A
): Property =
    forAllShrinkBlind(genB, { emptySequence() }, testable, prop)

/**
 * run a test with shrinking
 */
inline fun <reified A, B> forAllShrink(
    genB: Gen<B>,
    showB: Show<B> = Show.any(),
    noinline shrinkerB: (B) -> Sequence<B>,
    testable: Testable<A> = defTestable(),
    noinline prop: (B) -> A
): Property =
    forAllShrinkShow(genB, shrinkerB, { showB.run { it.show() } }, testable, prop)

/**
 * run a test with shrinking and a specific show function
 */
inline fun <reified A, B> forAllShrinkShow(
    genB: Gen<B>,
    noinline shrinkerB: (B) -> Sequence<B>,
    noinline showerB: (B) -> String,
    testable: Testable<A> = defTestable(),
    noinline prop: (B) -> A
): Property =
    forAllShrinkBlind(genB, shrinkerB) { x: B ->
        counterexample(showerB(x), prop(x))
    }

/**
 * run a test with shrinking but without showing a counterexample
 */
inline fun <reified A, B> forAllShrinkBlind(
    genB: Gen<B>,
    noinline shrinkerB: (B) -> Sequence<B>,
    testable: Testable<A> = defTestable(),
    noinline prop: (B) -> A
): Property =
    again(
        Property(
            Gen.monad().binding {
                shrinking(testable, shrinkerB, genB.bind(), prop).unProperty.bind()
            }.fix()
        )
    )

/**
 * Helper that looks up instances based on generics
 */
inline fun <reified A, reified B : Any> forAll(
    arbB: Arbitrary<B> = defArbitrary(),
    testable: Testable<A> = defTestable(),
    showB: Show<B> = defShow(),
    noinline prop: (B) -> A
): Property =
    forAll(arbB.arbitrary(), testable, showB, prop)

/**
 * Helper that looks up instances based on generics
 */
inline fun <reified A, reified B : Any> forAllBlind(
    arbB: Arbitrary<B> = defArbitrary(),
    testable: Testable<A> = defTestable(),
    noinline prop: (B) -> A
): Property =
    forAllBlind(arbB.arbitrary(), testable, prop)

/**
 * Helper that looks up instances based on generics
 */
inline fun <reified A, reified B : Any> forAllShrink(
    arbB: Arbitrary<B> = defArbitrary(),
    testable: Testable<A> = defTestable(),
    showB: Show<B> = defShow(),
    noinline prop: (B) -> A
): Property =
    forAllShrink(arbB.arbitrary(), showB, { arbB.shrink(it) }, testable, prop)

/**
 * Helper that looks up instances based on generics
 */
inline fun <reified A, reified B : Any> forAllShrinkBlind(
    arbB: Arbitrary<B> = defArbitrary(),
    testable: Testable<A> = defTestable(),
    noinline prop: (B) -> A
): Property =
    forAllShrinkBlind(arbB.arbitrary(), { arbB.shrink(it) }, testable, prop)

/**
 * test an io property (does not shrink)
 */
inline fun <reified A> ioProperty(propIO: IO<A>, testable: Testable<A> = defTestable()): Property =
    idempotentIOProperty(
        propIO.map { noShrinking(it) }
    )

/**
 * test an io property with shrinking
 */
inline fun <reified A> idempotentIOProperty(propIO: IO<A>, testable: Testable<A> = defTestable()): Property =
    Property(
        propIO.map {
            testable.run { it.property() }.unProperty
        }.promote(IO.monad()).map { Prop(ioRose(it.fix().map { it.unProp })) }
    )