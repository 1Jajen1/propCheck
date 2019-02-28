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
import io.jannis.propTest.Gen
import io.jannis.propTest.assertions.property.testable.testable
import io.jannis.propTest.assertions.rose.monad.monad
import io.jannis.propTest.assertions.testresult.testable.testable
import io.jannis.propTest.fix

sealed class Callback {
    class PostTest(val kind: CallbackKind, val fn: (State, TestResult) -> IO<Unit>) : Callback()
    class PostFinalFailure(val kind: CallbackKind, val fn: (State, TestResult) -> IO<Unit>) : Callback()
}

sealed class CallbackKind {
    object Counterexample : CallbackKind()
    object NoCounterexample : CallbackKind()
}

data class TestResult(
    val ok: Option<Boolean>,
    val expected: Boolean,
    val reason: String,
    val exception: Option<Throwable>,
    val abort: Boolean,
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

fun ioRose(rose: IO<Rose<TestResult>>): Rose<TestResult> = Rose.IORose(
    protectRose(rose)
)

fun protectRose(rose: IO<Rose<TestResult>>): IO<Rose<TestResult>> = rose.handleError {
    Rose.just(
        failed(
            reason = "Exception",
            exception = it.some()
        )
    )
}

fun <A> onRose(rose: Rose<A>, f: (A, Sequence<Rose<A>>) -> Rose<A>): Rose<A> = when (rose) {
    is Rose.MkRose -> f(rose.res, rose.shrunk)
    is Rose.IORose -> Rose.IORose(rose.ioRose.map { onRose(it, f) })
}

fun protectResult(io: IO<TestResult>): IO<TestResult> = io.handleError {
    failed(
        reason = "Exception",
        exception = it.some()
    )
}

fun protectResults(rose: Rose<TestResult>): Rose<TestResult> = onRose(rose) { x, rs ->
    Rose.IORose(
        IO.monad().fx {
            val y = protectResult(IO.just(x)).bind()
            Rose.MkRose(y, rs.map(::protectResults))
        }.fix()
    )
}

fun reduceRose(rose: Rose<TestResult>): IO<Rose<TestResult>> = when (rose) {
    is Rose.IORose -> rose.ioRose.flatMap(::reduceRose)
    is Rose.MkRose -> IO.just(rose)
}

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

fun <A> mapResult(testableA: Testable<A>, f: (TestResult) -> TestResult): (A) -> Property =
    mapRoseResult(testableA) { protectResults(it.map(f)) }

fun <A> mapTotalResult(testableA: Testable<A>, f: (TestResult) -> TestResult): (A) -> Property =
    mapRoseResult(testableA) { it.map(f) }

fun <A> mapRoseResult(testableA: Testable<A>, f: (Rose<TestResult>) -> Rose<TestResult>): (A) -> Property =
    mapProp(testableA) { Prop(f(it.unProp)) }

fun <A> mapProp(testableA: Testable<A>, f: (Prop) -> Prop): (A) -> Property = { a ->
    testableA.run {
        Property(a.property().unProperty.map(f))
    }
}

fun <A> mapSize(testableA: Testable<A>, f: (Int) -> Int): (A) -> Property = { a ->
    testableA.run {
        Property(a.property().unProperty.scale(f))
    }
}

fun <A, B> shrinking(testableA: Testable<A>, shrink: (B) -> Sequence<B>, arg: B, pf: (B) -> A): Property {
    fun props(b: B, alreadyTested: Set<B>): Rose<Gen<Prop>> = Rose.MkRose(
        testableA.run { pf(b).property().unProperty },
        shrink(b).filter { alreadyTested.contains(it).not() }.let {
            it.mapIndexed { i, v -> props(v, setOf(b) + it.take(i).toSet() + alreadyTested) }
        }
    )
    return Property(props(arg, setOf(arg)).promote(Rose.monad()).map { Prop(joinRose(it.fix().map { it.unProp })) })
}

fun <A> noShrinking(testableA: Testable<A>): (A) -> Property =
    mapRoseResult(testableA) {
        onRose(it) { res, _ -> Rose.MkRose(res, emptySequence()) }
    }

fun <A> counterexample(testableA: Testable<A>, s: String): (A) -> Property =
    mapTotalResult(Property.testable()) { res ->
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
    }.compose(callback(testableA, Callback.PostFinalFailure(CallbackKind.Counterexample) { st, res ->
        st.output.update {
            it + s + "\n"
        }.fix()
    }))

fun <A> expectFailure(testableA: Testable<A>): (A) -> Property =
    mapTotalResult(testableA) { res ->
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

fun <A> once(testableA: Testable<A>): (A) -> Property =
    mapTotalResult(testableA) { res ->
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

fun <A> again(testableA: Testable<A>): (A) -> Property =
    mapTotalResult(testableA) { res ->
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

fun <A> withMaxSuccess(testableA: Testable<A>, maxSuccess: Int): (A) -> Property =
    mapTotalResult(testableA) { res ->
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

fun <A> checkCoverage(testableA: Testable<A>, confidence: Confidence = Confidence()): (A) -> Property =
    mapTotalResult(testableA) { res ->
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

fun <A> label(testableA: Testable<A>, label: String): (A) -> Property =
    mapTotalResult(testableA) { res ->
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

fun <A, B> collect(testableA: Testable<A>, showB: Show<B>, b: B): (A) -> Property =
    label(testableA, showB.run { b.show() })

fun <A> classify(testableA: Testable<A>, bool: Boolean, label: String): (A) -> Property =
    if (!bool) { a: A -> testableA.run { a.property() } }
    else mapTotalResult(testableA) { res ->
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

fun <A> cover(testableA: Testable<A>, p: Double, bool: Boolean, label: String): (A) -> Property =
    mapTotalResult(Property.testable()) { res ->
        TestResult(
            ok = res.ok,
            reason = res.reason,
            exception = res.exception,
            requiredCoverage = listOf(Tuple3(none<String>(), label, p / 100)) + res.requiredCoverage,
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
    }.compose(classify(testableA, bool, label))

fun <A> tabulate(testableA: Testable<A>, key: String, values: List<String>): (A) -> Property =
    mapTotalResult(testableA) { res ->
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

fun <A> coverTable(testableA: Testable<A>, key: String, values: List<Tuple2<String, Double>>): (A) -> Property =
    mapTotalResult(testableA) { res ->
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

fun <A> callback(testableA: Testable<A>, cb: Callback): (A) -> Property =
    mapTotalResult(testableA) { res ->
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

fun <A> whenFail(testableA: Testable<A>, f: () -> Unit): (A) -> Property =
    whenFailIO(testableA, IO { f() })

fun <A> whenFailIO(testableA: Testable<A>, f: IO<Unit>): (A) -> Property =
    callback(testableA, Callback.PostFinalFailure(CallbackKind.NoCounterexample) { _, _ -> f })

fun <A> whenFailEvery(testableA: Testable<A>, f: () -> Unit): (A) -> Property =
    whenFailEveryIO(testableA, IO { f() })

fun <A> whenFailEveryIO(testableA: Testable<A>, f: IO<Unit>): (A) -> Property =
    callback(testableA, Callback.PostTest(CallbackKind.NoCounterexample) { _, res ->
        if (res.ok == false.some()) f
        else IO.unit
    })

fun <A> verbose(testableA: Testable<A>): (A) -> Property {
    fun status(res: TestResult): String = when (res.ok) {
        true.some() -> "Passed"
        false.some() -> "Failed"
        None -> "Skipped (precondition false)"
        else -> throw IllegalStateException("Not possible")
    }

    fun newCb(cbs: List<Callback>): Callback = Callback.PostTest(CallbackKind.Counterexample) { st, res ->
        IO.monad().fx {
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
    return mapResult(testableA) { res ->
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

fun <A> verboseShrinking(testableA: Testable<A>): (A) -> Property {
    fun newCb(cbs: List<Callback>): Callback = Callback.PostTest(CallbackKind.Counterexample) { st, res ->
        if (res.ok == false.some())
            IO.monad().fx {
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
    return mapResult(testableA) { res ->
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

// ==>
fun <A> assert(testableA: Testable<A>, bool: Boolean): (A) -> Property =
    if (bool) { a: A -> testableA.run { a.property() } }
    else { a: A -> TestResult.testable().run { rejected().property() } }

// --------
fun <A, B> forAll(testableA: Testable<A>, showB: Show<B>, genB: Gen<B>): ((B) -> A) -> Property =
    forAllShrink(testableA, showB, genB) { emptySequence() }

fun <A, B> forAllBlind(testableA: Testable<A>, genB: Gen<B>): ((B) -> A) -> Property =
    forAllShrinkBlind(testableA, genB) { emptySequence() }

fun <A, B> forAllShrink(
    testableA: Testable<A>,
    showB: Show<B>,
    genB: Gen<B>,
    shrinkerB: (B) -> Sequence<B>
): ((B) -> A) -> Property =
    forAllShrinkShow(testableA, genB, shrinkerB, { showB.run { it.show() } })

fun <A, B> forAllShrinkShow(
    testableA: Testable<A>,
    genB: Gen<B>,
    shrinkerB: (B) -> Sequence<B>,
    showerB: (B) -> String
): ((B) -> A) -> Property = { pf ->
    forAllShrinkBlind(Property.testable(), genB, shrinkerB).invoke { x ->
        counterexample(testableA, showerB(x)).invoke(pf(x))
    }
}

fun <A, B> forAllShrinkBlind(
    testableA: Testable<A>,
    genB: Gen<B>,
    shrinkerB: (B) -> Sequence<B>
): ((B) -> A) -> Property = { pf ->
    again(Property.testable()).invoke(
        Property(
            Gen.monad().fx {
                shrinking(testableA, shrinkerB, genB.bind(), pf).unProperty.bind()
            }.fix()
        )
    )
}