package propCheck.property

import arrow.core.*
import arrow.optics.optics
import pretty.Doc
import pretty.doc
import pretty.nil
import pretty.text
import propCheck.*

/**
 * A single tests result
 */
@optics
data class TestResult(
    val ok: Option<Boolean>, // Some(true) => success, Some(false) => failure and None => discarded
    val expected: Boolean, // expected outcome
    val reason: Doc<Nothing>, // failure reason
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

internal fun defaultRes(): TestResult = TestResult(
    ok = none(),
    expected = true,
    reason = nil(),
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

fun failed(reason: Doc<Nothing>, exception: Option<Throwable> = none()): TestResult =
    TestResult.optionException.set(
        TestResult.reason.set(
            TestResult.ok.set(defaultRes(), false), reason
        ), exception
    )

fun rejected(): TestResult = TestResult.optionOk.set(defaultRes(), none())

fun liftBoolean(bool: Boolean): TestResult = if (bool) succeeded() else failed(
    reason = "Falsifiable".text()
)
