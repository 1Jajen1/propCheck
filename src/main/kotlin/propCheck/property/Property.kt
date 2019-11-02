package propCheck.property

import arrow.core.*
import arrow.core.extensions.eval.monad.monad
import arrow.core.extensions.fx
import arrow.core.extensions.list.traverse.traverse
import arrow.core.extensions.sequence.traverse.traverse
import arrow.core.extensions.sequencek.foldable.foldRight
import arrow.extension
import arrow.fx.ForIO
import arrow.fx.IO
import arrow.fx.extensions.fx
import arrow.fx.extensions.io.monad.monad
import arrow.fx.fix
import arrow.typeclasses.Eq
import arrow.typeclasses.Show
import pretty.*
import propCheck.*
import propCheck.arbitrary.Arbitrary
import propCheck.arbitrary.Gen
import propCheck.arbitrary.fix
import propCheck.arbitrary.gen.applicative.applicative
import propCheck.arbitrary.gen.monad.flatMap
import propCheck.arbitrary.gen.monad.monad
import propCheck.property.property.testable.testable
import propCheck.property.rose.monad.monad
import propCheck.property.testresult.testable.testable

data class Property(val unProperty: Gen<Prop>) {
    companion object
}

data class Prop(val unProp: Rose<ForIO, TestResult>)

/**
 * Base interface for testable data
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

@extension
interface OptionTestable<A> : Testable<Option<A>> {
    fun TA(): Testable<A>

    override fun Option<A>.property(): Property = fold({
        TestResult.testable().run { rejected().property() }
    }, {
        TA().run { it.property() }
    })
}

/**
 * Callbacks. Can perform io based on state and results
 */
sealed class Callback {
    class PostTest(val kind: CallbackKind, val fn: (State, TestResult) -> PropCheck<Unit>) : Callback()
    class PostFinalFailure(val kind: CallbackKind, val fn: (State, TestResult) -> PropCheck<Unit>) : Callback()
}

sealed class CallbackKind {
    object Counterexample : CallbackKind()
    object NoCounterexample : CallbackKind()
}


// Combinators for building properties
/**
 * Map over the test result of a property and protect the results with error handlers
 */
fun mapResult(
    a: Property,
    f: (TestResult) -> TestResult
): Property = mapRoseResult(a) { protectResults(it.map(IO.monad(), f)) }

/**
 * Map over the test result of a property. f must be total
 */
fun mapTotalResult(
    a: Property,
    f: (TestResult) -> TestResult
): Property = mapRoseResult(a) { it.map(IO.monad(), f) }

/**
 * Map over a rose containing the TestResult, f must be total
 */
fun mapRoseResult(
    a: Property,
    f: (Rose<ForIO, TestResult>) -> Rose<ForIO, TestResult>
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
    ).promote(Rose.monad(IO.monad())).map {
        Prop(
            it.fix().flatMap(IO.monad()) {
                it.unProp
            }
        )
    })

internal fun <B> props(
    shrink: (B) -> Sequence<B>,
    pf: (B) -> Property,
    b: B
): Rose<ForIO, Gen<Prop>> =
    Rose(
        IO.just(
            RoseF(
                pf(b).unProperty,
                // this is a bit ugly, is there a better way to shrink lazy?
                sequenceOf(Unit).flatMap { shrink(b).map { v -> props(shrink, pf, v) } }
            )
        )
    )

/**
 * disable shrinking in a property by just supplying empty sequences
 */
fun noShrinking(a: Property): Property =
    mapRoseResult(a) {
        onRose(it) { res, _ -> Rose.just(IO.monad(), res) }
    }

/**
 * print out a counterexample on failure
 */
fun counterexample(s: () -> String, a: Property): Property =
    mapTotalResult(
        callback(Callback.PostFinalFailure(CallbackKind.Counterexample) { st, _ ->
            writeDoc(s().doc())
        }, a)
    ) { res ->
        TestResult.testCase.modify(res) { listOf(s()) + it }
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
    callback(Callback.PostFinalFailure(CallbackKind.NoCounterexample) { _, _ ->
        liftIO(
            f
        )
    }, a)

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
        if (res.ok == false.some()) liftIO(f)
        else liftIO(IO.unit)
    }, a)

/**
 * append a lot more output
 */
fun verbose(a: Property): Property =
    mapResult(a) { res ->
        TestResult.callbacks.modify(res) { listOf(newCb(it)) + it }
    }

internal fun status(res: TestResult): Doc<Nothing> = when (res.ok) {
    true.some() -> "Passed"
    false.some() -> "Failed"
    None -> "Skipped (precondition false)"
    else -> throw IllegalStateException("Not possible")
}.text()

internal fun newCb(cbs: List<Callback>): Callback =
    Callback.PostTest(CallbackKind.Counterexample) { st, res ->
        propCheckFx {
            !writeDoc((status(res) + colon() + res.testCase.map { it.doc<Nothing>() }.punctuate(comma()).hCat()))

            !cbs.filter { it is Callback.PostFinalFailure && it.kind == CallbackKind.Counterexample }
                .traverse(propCheckMonad()) {
                    (it as Callback.PostFinalFailure).fn(st, res)
                }
            Unit
        }
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
            propCheckFx {
                !writeDoc(("Failed:".text<Nothing>() spaced  res.testCase.map { it.doc<Nothing>() }.punctuate(comma()).hSep()))

                !cbs.filter { it is Callback.PostFinalFailure && it.kind == CallbackKind.Counterexample }
                    .traverse(propCheckMonad()) {
                        (it as Callback.PostFinalFailure).fn(st, res)
                    }

                Unit
            }
        else liftIO(IO.unit)
    }

/**
 * discared data based on boolean
 */
fun discardIf(bool: Boolean, a: Eval<Property>): Property =
    if (!bool) a.value()
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
        counterexample({ showerB(x) }, testA.run { prop(x).property() })
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
            Gen.monad().fx.monad {
                shrinking(shrinkerB, genB.bind(), testA.run { prop.andThen { it.property() } })
                    .unProperty.bind()
            }.fix()
        )
    )

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
                    { if (bool) "Left" else "Right" },
                    if (bool) a else b
                ).unProperty
            }
        )
    )

fun and(
    a: Property,
    b: () -> Property
): Property =
    conjoin(
        sequenceOf(
            Eval.now(a),
            Eval.later(b)
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
            Gen.monad().fx.monad {
                val roses = props.traverse(Gen.applicative()) {
                    it.map {
                        it.unProperty.map { it.unProp }
                    }.promote(Eval.monad())
                }.bind()
                Prop(conj(roses.fix().map { it.fix() }, ::identity))
            }.fix()
        )
    )

internal fun conj(roses: Sequence<Eval<Rose<ForIO, TestResult>>>, k: (TestResult) -> TestResult): Rose<ForIO, TestResult> = when {
    roses.firstOrNull() == null -> Rose.just(IO.monad(), k(succeeded()))
    else -> Rose(
        IO.fx {
            val reduced = !roses.first().value().runRose
            if (reduced.res.expected.not())
                RoseF(
                    failed("expect failure may not be used inside a conjunction".text()),
                    emptySequence()
                )
            else
                when (reduced.res.ok) {
                    true.some() -> !conj(roses.drop(1)) {
                        addLabels(
                            reduced.res,
                            addCallbacksAndCoverage(reduced.res, k(it))
                        )
                    }.runRose
                    false.some() -> reduced
                    None -> IO.fx {
                        val reduced2 = !conj(roses.drop(1)) {
                            addCallbacksAndCoverage(
                                reduced.res,
                                k(it)
                            )
                        }.runRose
                        when (reduced2.res.ok) {
                            true.some() -> RoseF(
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
        }
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
    b: () -> Property
): Property =
    disjoin(
        sequenceOf(
            Eval.now(a),
            Eval.later(b)
        )
    )

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
            Gen.monad().fx.monad {
                val roses = props.traverse(Gen.applicative()) {
                    it.map {
                        it.unProperty.map { it.unProp }
                    }.promote(Eval.monad())
                }.bind()
                Prop(
                    roses.foldRight(
                        Eval.now(
                            Rose.just(
                                IO.monad(),
                                failed(nil())
                            )
                        )
                    ) { r, acc ->
                        Eval.fx {
                            disj(r.fix().bind(), acc)
                        }
                    }.value()
                )
            }.fix()
        )
    )

internal fun disj(p: Rose<ForIO, TestResult>, q: Eval<Rose<ForIO, TestResult>>): Rose<ForIO, TestResult> = Rose.monad(IO.monad()).fx.monad {
    val res1 = p.bind()
    if (res1.expected.not())
        failed("expectFailure may not occur inside a disjunction".text())
    else
        when (res1.ok) {
            false.some() -> {
                val res2 = q.value().bind()
                if (res2.expected.not())
                    failed("expectFailure may not occur inside a disjunction".text())
                else
                    when (res2.ok) {
                        true.some() -> addCoverage(res1, res2)
                        false.some() -> TestResult(
                            ok = false.some(),
                            expected = true,
                            reason = res1.reason + hardLine() + res2.reason,
                            exception = res1.exception.or(res2.exception),
                            abort = false,
                            optionNumOfTests = none(),
                            optionCheckCoverage = none(),
                            labels = emptyList(),
                            classes = emptyList(),
                            tables = emptyList(),
                            requiredCoverage = emptyList(),
                            callbacks = res1.callbacks + res2.callbacks,
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

fun <A> A.eqv(b: A, eqA: Eq<A> = Eq.any(), showA: Show<A> = Show.any()): Property =
    counterexample(
        {
            "Expected: ${showA.run { this@eqv.show() }} to be equal to:\n" +
                    "        : ${showA.run { b.show() }}"
        },
        eqA.run { this@eqv.eqv(b) }
    )

fun <A> A.neqv(b: A, eqA: Eq<A> = Eq.any(), showA: Show<A> = Show.any()): Property =
    counterexample(
        {
            "Expected: ${showA.run { this@neqv.show() }} to not be equal to:\n" +
                    "        : ${showA.run { b.show() }}"
        },
        eqA.run { this@neqv.neqv(b) }
    )
