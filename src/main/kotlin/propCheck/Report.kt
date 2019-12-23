package propCheck

import pretty.*
import propCheck.arbitrary.RandSeed
import propCheck.property.Markup

data class Report(
    val numTests: Int,
    val numDiscarded: Int,
    val result: Result
) {
    companion object
}

sealed class Result {
    object Success : Result()
    object GivenUp : Result()
    data class Failure(val summary: FailureSummary) : Result()

    companion object
}

data class FailureSummary(
    val usedSize: Int,
    val usedSeed: RandSeed,
    val numShrinks: Int,
    val failureDoc: Doc<Markup>,
    val annotations: List<Doc<Markup>>,
    val footnotes: List<Doc<Markup>>
) {
    companion object
}

// TODO in the future add terminal specific code here for size/color/etc
// TODO refractor common things out and provide nicer formatting
fun renderReport(report: Report): String = when (report.result) {
    is Result.Success -> "*** Success: Passed".text<Markup>() spaced report.numTests.doc() spaced report.numTests.plural<Markup>(
        "test".text(),
        "tests".text()
    ) +
            (if (report.numDiscarded > 0) (report.numDiscarded.doc<Markup>() spaced "discarded".text()).enclose(
                space<Markup>() + lParen(),
                rParen()
            ) else nil())
    is Result.GivenUp -> "*** Gave up: After".text<Markup>() spaced report.numTests.doc() spaced report.numTests.plural<Markup>(
        "test".text(),
        "tests".text()
    ) +
            (if (report.numDiscarded > 0) (report.numDiscarded.doc<Markup>() spaced "discarded".text()).enclose(
                space<Markup>() + lParen(),
                rParen()
            ) else nil())
    is Result.Failure -> "*** Failed: After".text<Markup>() spaced report.numTests.doc() spaced report.numTests.plural<Markup>(
        "test".text(),
        "tests".text()
    ) +
            (if (report.result.summary.numShrinks > 0)
                space<Markup>() + "and".text() spaced report.result.summary.numShrinks.doc() spaced report.result.summary.numShrinks.plural(
                    "shrink".text(),
                    "shrinks".text()
                )
            else nil()) +
            (if (report.numDiscarded > 0) (report.numDiscarded.doc<Markup>() spaced "discarded".text()).enclose(
                space<Markup>() + lParen(),
                rParen()
            ) else nil()) + softLine() +
            (if (report.result.summary.annotations.isNotEmpty()) report.result.summary.annotations.vSep() + hardLine() else nil()) +
            (if (report.result.summary.failureDoc.unDoc is DocF.Nil) nil() else report.result.summary.failureDoc + hardLine()) +
            (if (report.result.summary.footnotes.isNotEmpty()) report.result.summary.footnotes.vSep() + hardLine() else nil()) +
            ("Repeat this test with:".text<Markup>() softLine "recheck(".text() softLineBreak
            ("size =".text<Markup>() spaced report.result.summary.usedSize.doc<Markup>() + comma() softLine
                    "seed =".text() spaced "RandSeed(".text<Markup>() + report.result.summary.usedSeed.seed.doc() + comma() spaced
                    report.result.summary.usedSeed.gamma.doc<Markup>() + rParen() + comma() softLine
                    "prop = ...".text() softLine rParen()
                    )).nest(7)
}
    .renderPretty()
    .renderString()

fun <A> Int.plural(s: Doc<A>, p: Doc<A>): Doc<A> = when (this) {
    1 -> s
    else -> p
}

