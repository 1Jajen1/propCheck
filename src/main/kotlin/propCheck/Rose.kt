package propCheck

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.eval.monad.monad
import arrow.data.extensions.list.traverse.traverse
import arrow.data.extensions.sequence.traverse.traverse
import arrow.data.extensions.sequencek.foldable.foldRight
import arrow.data.fix
import arrow.effects.IO
import arrow.effects.extensions.io.applicative.applicative
import arrow.effects.extensions.io.applicativeError.handleError
import arrow.effects.extensions.io.monad.monad
import arrow.effects.fix
import arrow.extension
import arrow.optics.optics
import arrow.typeclasses.*
import propCheck.arbitrary.Arbitrary
import propCheck.arbitrary.Gen
import propCheck.arbitrary.fix
import propCheck.arbitrary.gen.applicative.applicative
import propCheck.arbitrary.gen.monad.flatMap
import propCheck.arbitrary.gen.monad.monad
import propCheck.property.testable.testable
import propCheck.rose.monad.monad
import propCheck.testresult.testable.testable

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
    companion object {
        fun toTuple(res: TestResult) = res.run {
            Tuple13(
                ok,
                expected,
                reason,
                exception,
                abort,
                optionNumOfTests,
                optionCheckCoverage,
                labels,
                classes,
                tables,
                requiredCoverage,
                testCase,
                callbacks
            )
        }

        fun fromTuple(tup: Tuple13<Option<Boolean>, Boolean, String, Option<Throwable>, Boolean, Option<Int>, Option<Confidence>, List<String>, List<String>, List<Tuple2<String, String>>, List<Tuple3<Option<String>, String, Double>>, List<String>, List<Callback>>) =
            TestResult(
                tup.a,
                tup.b,
                tup.c,
                tup.d,
                tup.e,
                tup.f,
                tup.g,
                tup.h,
                tup.i,
                tup.j,
                tup.k,
                tup.l,
                tup.m
            )
    }
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
fun ioRose(rose: IO<Rose<TestResult>>): Rose<TestResult> =
    Rose.IORose(
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
fun protectResults(rose: Rose<TestResult>): Rose<TestResult> =
    onRose(rose) { x, rs ->
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

/**
 * Map over the test result of a property and protect the results with error handlers
 */
fun mapResult(
    a: Property,
    f: (TestResult) -> TestResult
): Property = mapRoseResult(a) { protectResults(it.map(f)) }

/**
 * Map over the test result of a property. f must be total
 */
fun mapTotalResult(
    a: Property,
    f: (TestResult) -> TestResult
): Property = mapRoseResult(a) { it.map(f) }

/**
 * Map over a rose containing the TestResult, f must be total
 */
fun mapRoseResult(
    a: Property,
    f: (Rose<TestResult>) -> Rose<TestResult>
): Property = mapProp(a) { Prop(f(it.unProp)) }

/**
 * map over a Prop (wrapper around rose). f must be total
 */
fun mapProp(a: Property, f: (Prop) -> Prop): Property =
    Property(a.unProperty.map(f))

/**
 * Map over the size of a generator used with the property. Calling Gen.scale(f)
 */
fun mapSize(a: Property, f: (Int) -> Int): Property =
    Property(a.unProperty.scale(f))

/**
 * enable shrinking in a property
 */
fun <B> shrinking(
    shrink: (B) -> Sequence<B>,
    arg: B,
    pf: (B) -> Property
): Property = Property(
    props(
        shrink,
        pf,
        arg
    ).promote(Rose.monad()).map { Prop(joinRose(it.fix().map { it.unProp })) })

internal fun <B> props(
    shrink: (B) -> Sequence<B>,
    pf: (B) -> Property,
    b: B
): Rose<Gen<Prop>> = Rose.MkRose(
    pf(b).unProperty,
    shrink(b).let {
        it.mapIndexed { i, v -> props(shrink, pf, v) }
    }
)

/**
 * disable shrinking in a property by just supplying empty sequences
 */
fun noShrinking(a: Property): Property =
    mapRoseResult(a) {
        onRose(it) { res, _ -> Rose.MkRose(res, emptySequence()) }
    }

/**
 * print out a counterexample on failure
 */
fun counterexample(s: String, a: Property): Property =
    mapTotalResult(
        callback(Callback.PostFinalFailure(CallbackKind.Counterexample) { st, _ ->
            st.output.update {
                it + s + "\n"
            }.fix()
        }, a)
    ) { res ->
        TestResult.testCase.modify(res) { listOf(s) + it }
    }

/**
 * expect the property to fail
 */
fun expectFailure(a: Property): Property =
    mapTotalResult(a) { res ->
        TestResult.expected.set(res, false)
    }

/**
 * Abort after one execution
 */
fun once(a: Property): Property =
    mapTotalResult(a) { res ->
        TestResult.abort.set(res, true)
    }

/**
 * Continue to execute test if needed
 */
fun again(a: Property): Property =
    mapTotalResult(a) { res ->
        TestResult.abort.set(res, false)
    }

/**
 * Change the maximum tested amount
 */
fun withMaxSuccess(maxSuccess: Int, a: Property): Property =
    mapTotalResult(a) { res ->
        TestResult.optionNumOfTests.set(res, maxSuccess)
    }

/**
 * enable coverage checks
 * Without tests will not fail (only print warnings) when coverage is not satisfied. Using this will make tests fail
 */
fun checkCoverage(
    a: Property,
    confidence: Confidence = Confidence()
): Property =
    mapTotalResult(a) { res ->
        TestResult.optionCheckCoverage.set(res, confidence)
    }

/**
 * Append a label to a test case
 */
fun label(label: String, a: Property): Property =
    mapTotalResult(a) { res ->
        TestResult.labels.modify(res) { listOf(label) + it }
    }

/**
 * implicitly label all cases
 */
fun <B> collect(
    b: B,
    a: Property,
    showB: Show<B> = Show.any()
): Property =
    label(showB.run { b.show() }, a)

/**
 * classify tests based on some boolean value
 */
fun classify(bool: Boolean, label: String, a: Property): Property =
    if (!bool) a
    else mapTotalResult(a) { res ->
        TestResult.classes.modify(res) { listOf(label) + it }
    }

/**
 * add required coverage based on a boolean prop for a specific label
 */
fun cover(
    p: Double,
    bool: Boolean,
    label: String,
    a: Property
): Property =
    mapTotalResult(
        classify(bool, label, a)
    ) { res ->
        TestResult.requiredCoverage.modify(res) { listOf(Tuple3(none<String>(), label, p / 100)) + it }
    }

/**
 * tabulate data
 */
fun tabulate(
    key: String,
    values: List<String>,
    a: Property
): Property =
    mapTotalResult(a) { res ->
        TestResult.tables.modify(res) { values.map { key toT it } + it }
    }

/**
 * add required coverage based on tables
 */
fun coverTable(
    key: String,
    values: List<Tuple2<String, Double>>,
    a: Property
): Property =
    mapTotalResult(a) { res ->
        TestResult.requiredCoverage.modify(res) { values.map { Tuple3(key.some(), it.a, it.b / 100) } + it }
    }

/**
 * append a callback to the property
 */
fun callback(cb: Callback, a: Property): Property =
    mapTotalResult(a) { res ->
        TestResult.callbacks.modify(res) { it + listOf(cb) }
    }

/**
 * execute some code when a test fails
 */
fun whenFail(a: Property, f: () -> Unit): Property =
    whenFailIO(a, IO { f() })

/**
 * execute some io code when a test fails
 */
fun whenFailIO(a: Property, f: IO<Unit>): Property =
    callback(Callback.PostFinalFailure(CallbackKind.NoCounterexample) { _, _ -> f }, a)

/**
 * execute some code on every failure (not just the last one)
 */
fun whenFailEvery(a: Property, f: () -> Unit): Property =
    whenFailEveryIO(a, IO { f() })

/**
 * execute some io code on every failure (not just the last one)
 */
fun whenFailEveryIO(a: Property, f: IO<Unit>): Property =
    callback(Callback.PostTest(CallbackKind.NoCounterexample) { _, res ->
        if (res.ok == false.some()) f
        else IO.unit
    }, a)

/**
 * append a lot more output
 */
fun verbose(a: Property): Property =
    mapResult(a) { res ->
        TestResult.callbacks.modify(res) { listOf(newCb(it)) + it }
    }

internal fun status(res: TestResult): String = when (res.ok) {
    true.some() -> "Passed"
    false.some() -> "Failed"
    None -> "Skipped (precondition false)"
    else -> throw IllegalStateException("Not possible")
}

internal fun newCb(cbs: List<Callback>): Callback =
    Callback.PostTest(CallbackKind.Counterexample) { st, res ->
        IO.monad().binding {
            st.output.update {
                it + status(res) + ": " + res.testCase.joinToString() + "\n"
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
fun verboseShrinking(a: Property): Property =
    mapResult(a) { res ->
        TestResult.callbacks.modify(res) { listOf(newCb2(it)) + it }
    }

internal fun newCb2(cbs: List<Callback>): Callback =
    Callback.PostTest(CallbackKind.Counterexample) { st, res ->
        if (res.ok == false.some())
            IO.monad().binding {
                st.output.update {
                    it + "Failed: " + res.testCase.joinToString() + "\n"
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
fun discardIf(bool: Boolean, a: Property): Property =
    if (!bool) a
    else TestResult.testable().run { rejected().property() }

/**
 * run a test with a given generator and without shrinking
 */
fun <A, B> forAll(
    genB: Gen<B>,
    testA: Testable<A>,
    showB: Show<B> = Show.any(),
    prop: (B) -> A
): Property =
    forAllShrink(genB, { emptySequence() }, testA, showB, prop)

/**
 * run a test without printing a counterexample on failure
 */
fun <A, B> forAllBlind(
    genB: Gen<B>,
    testA: Testable<A>,
    prop: (B) -> A
): Property =
    forAllShrinkBlind(genB, { emptySequence() }, testA, prop)

/**
 * run a test with shrinking
 */
fun <A, B> forAllShrink(
    genB: Gen<B>,
    shrinkerB: (B) -> Sequence<B>,
    testA: Testable<A>,
    showB: Show<B> = Show.any(),
    prop: (B) -> A
): Property =
    forAllShrinkShow(genB, shrinkerB, { showB.run { it.show() } }, testA, prop)

/**
 * run a test with shrinking and a specific show function
 */
fun <A, B> forAllShrinkShow(
    genB: Gen<B>,
    shrinkerB: (B) -> Sequence<B>,
    showerB: (B) -> String,
    testA: Testable<A>,
    prop: (B) -> A
): Property =
    forAllShrinkBlind(genB, shrinkerB, Property.testable()) { x: B ->
        counterexample(showerB(x), testA.run { prop(x).property() })
    }

/**
 * run a test with shrinking but without showing a counterexample
 */
fun <A, B> forAllShrinkBlind(
    genB: Gen<B>,
    shrinkerB: (B) -> Sequence<B>,
    testA: Testable<A>,
    prop: (B) -> A
): Property =
    again(
        Property(
            Gen.monad().binding {
                shrinking(shrinkerB, genB.bind(), testA.run { prop.andThen { it.property() } }).unProperty.bind()
            }.fix()
        )
    )

inline fun <reified A>defTestable(): Testable<A> = when (A::class.qualifiedName) {
    Boolean::class.qualifiedName -> Boolean.testable()
    TestResult::class.qualifiedName -> TestResult.testable()
    Property::class.qualifiedName -> Property.testable()
    else -> throw IllegalArgumentException("Could not find default testable for ${A::class.qualifiedName}. This can only happen in forAll and forAllBlind, make sure to only return either Boolean, TestResult or Property there!")
} as Testable<A>

/**
 * Helper that looks up instances based on generics
 */
inline fun <reified A, reified B : Any> forAll(
    arbB: Arbitrary<B> = defArbitrary(),
    showB: Show<B> = defShow(),
    testA: Testable<A> = defTestable(),
    noinline prop: (B) -> A
): Property =
    forAllShrink(arbB.arbitrary(), { arbB.shrink(it) }, testA, showB, prop)

/**
 * Helper that looks up instances based on generics
 */
inline fun <reified A, reified B : Any> forAllBlind(
    arbB: Arbitrary<B> = defArbitrary(),
    testA: Testable<A> = defTestable(),
    noinline prop: (B) -> A
): Property =
    forAllShrinkBlind(arbB.arbitrary(), { arbB.shrink(it) }, testA, prop)

/**
 * test an io property (does not shrink)
 */
fun ioProperty(propIO: IO<Property>): Property =
    idempotentIOProperty(
        propIO.map { noShrinking(it) }
    )

/**
 * test an io property with shrinking
 */
fun idempotentIOProperty(propIO: IO<Property>): Property =
    Property(
        propIO.map {
            it.unProperty
        }.promote(IO.monad()).map { Prop(ioRose(it.fix().map { it.unProp })) }
    )

fun choice(
    a: Property,
    b: Property
): Property =
    again(
        Property(
            Gen.elements(false, true).flatMap { bool ->
                counterexample(
                    if (bool) "Left" else "Right",
                    if (bool) a else b
                ).unProperty
            }
        )
    )

fun and(
    a: Property,
    b: Eval<Property>
): Property =
    conjoin(
        sequenceOf(
            Eval.now(a),
            b
        )
    )

fun conjoin(props: Sequence<Eval<Property>>): Property =
    again(
        Property(
            Gen.monad().binding {
                val roses = props.traverse(Gen.applicative()) {
                    it.map {
                        it.unProperty.map { it.unProp }
                    }.promote(Eval.monad())
                }.bind()
                Prop(conj(roses.fix().map { it.fix() }, ::identity))
            }.fix()
        )
    )

internal fun conj(roses: Sequence<Eval<Rose<TestResult>>>, k: (TestResult) -> TestResult): Rose<TestResult> = when {
    roses.firstOrNull() == null -> Rose.MkRose(k(succeeded()), emptySequence())
    else -> Rose.IORose(
        IO.monad().binding {
            val reduced = when (val it = reduceRose(roses.first().value()).bind()) {
                is Rose.MkRose -> it
                is Rose.IORose -> throw IllegalStateException("The impossible happened")
            }
            if (reduced.res.expected.not())
                Rose.just(failed("expect failure may not be used inside a conjunction"))
            else
                when (reduced.res.ok) {
                    true.some() -> conj(roses.drop(1)) {
                        addLabels(reduced.res, addCallbacksAndCoverage(reduced.res, k(it)))
                    }
                    false.some() -> reduced
                    None -> IO.monad().binding {
                        val reduced2 = when (val it =
                            reduceRose(conj(roses.drop(1)) {
                                addCallbacksAndCoverage(
                                    reduced.res,
                                    k(it)
                                )
                            }).bind()) {
                            is Rose.MkRose -> it
                            is Rose.IORose -> throw IllegalStateException("The impossible happened")
                        }
                        when (reduced2.res.ok) {
                            true.some() -> Rose.MkRose(
                                TestResult.optionOk.set(
                                    reduced2.res,
                                    none()
                                ), emptySequence()
                            )
                            false.some() -> reduced2
                            None -> reduced2
                            else -> throw IllegalStateException("The impossible happened")
                        }
                    }.bind()
                    else -> throw IllegalStateException("The impossible happened")
                }
        }.fix()
    )
}

internal fun addCallbacksAndCoverage(result: TestResult, r: TestResult): TestResult =
    TestResult.callbacks.modify(
        TestResult.requiredCoverage.modify(r) { result.requiredCoverage + it }
    ) { result.callbacks + it }

internal fun addLabels(result: TestResult, r: TestResult): TestResult =
    TestResult.labels.modify(
        TestResult.classes.modify(
            TestResult.tables.modify(r) { result.tables + it }
        ) { result.classes + it }
    ) { result.labels + it }

fun or(
    a: Property,
    b: Eval<Property>
): Property =
    disjoin(
        sequenceOf(
            Eval.now(a),
            b
        )
    )

fun disjoin(props: Sequence<Eval<Property>>): Property =
    again(
        Property(
            Gen.monad().binding {
                val roses = props.traverse(Gen.applicative()) {
                    it.map {
                        it.unProperty.map { it.unProp }
                    }.promote(Eval.monad())
                }.bind()
                Prop(
                    roses.foldRight(Eval.now(Rose.just(failed("")))) { r, acc ->
                        Eval.monad().binding {
                            disj(r.fix().bind(), acc)
                        }.fix()
                    }.value()
                )
            }.fix()
        )
    )

internal fun disj(p: Rose<TestResult>, q: Eval<Rose<TestResult>>): Rose<TestResult> = Rose.monad().binding {
    val res1 = p.bind()
    if (res1.expected.not())
        failed("expectFailure may not occur inside a disjunction")
    else
        when (res1.ok) {
            false.some() -> {
                val res2 = q.value().bind()
                if (res2.expected.not())
                    failed("expectFailure may not occur inside a disjunction")
                else
                    when (res2.ok) {
                        true.some() -> addCoverage(res1, res2)
                        false.some() -> TestResult(
                            ok = false.some(),
                            expected = true,
                            reason = listOf(res1.reason, res2.reason).filter { it.isNotEmpty() }.joinToString(),
                            exception = res1.exception.or(res2.exception),
                            abort = false,
                            optionNumOfTests = none(),
                            optionCheckCoverage = none(),
                            labels = emptyList(),
                            classes = emptyList(),
                            tables = emptyList(),
                            requiredCoverage = emptyList(),
                            callbacks = res1.callbacks +
                                    (if (res1.callbacks.size + res2.callbacks.size > 0) listOf(
                                        Callback.PostFinalFailure(
                                            CallbackKind.Counterexample
                                        ) { st, _ ->
                                            st.output.update { it + "\n" }.fix()
                                        }) else emptyList()) +
                                    res2.callbacks,
                            testCase = res1.testCase + res2.testCase
                        )
                        else -> res2
                    }
            }
            else -> res1
        }
}.fix()

internal fun addCoverage(r: TestResult, s: TestResult): TestResult =
    TestResult.requiredCoverage.modify(s) { r.requiredCoverage + it }

fun <A>A.eqv(b: A, eqA: Eq<A> = Eq.any(), showA: Show<A> = Show.any()): Property =
    counterexample(
        "Expected: ${showA.run { this@eqv.show() }} to be equal to:\n" +
                "        : ${showA.run { b.show() }}",
        eqA.run { this@eqv.eqv(b) }
    )

fun <A>A.neqv(b: A, eqA: Eq<A> = Eq.any(), showA: Show<A> = Show.any()): Property =
    counterexample(
        "Expected: ${showA.run { this@neqv.show() }} to not be equal to:\n" +
                "        : ${showA.run { b.show() }}",
        eqA.run { this@neqv.neqv(b) }
    )