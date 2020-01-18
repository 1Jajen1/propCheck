package propCheck

import arrow.core.*
import arrow.syntax.collections.tail
import pretty.*
import pretty.ansistyle.monoid.monoid
import pretty.symbols.bullet
import pretty.symbols.comma
import pretty.symbols.dot
import propCheck.arbitrary.RandSeed
import propCheck.property.IconType
import propCheck.property.Markup
import propCheck.property.PropertyName
import propCheck.property.Size

data class Report<out A>(
    val numTests: Int,
    val numDiscarded: Int,
    val status: A
) {
    companion object
}

sealed class Progress {
    object Running : Progress()
    data class Shrinking(val report: FailureSummary) : Progress()
}

sealed class Result {
    object Success : Result()
    object GivenUp : Result()
    data class Failure(val summary: FailureSummary) : Result()

    companion object
}

data class FailureSummary(
    val usedSize: Size,
    val usedSeed: RandSeed,
    val numShrinks: Int,
    val failureDoc: Doc<Markup>,
    val annotations: List<Doc<Markup>>,
    val footnotes: List<Doc<Markup>>
) {
    companion object
}

data class FullTestReport(
    val successful: ListK<Report<Result>>,
    val failure: Option<Report<Result>>
)

// ------- Pretty printing
fun Option<PropertyName>.doc(): Doc<Markup> = fold({
    "<interactive>".text()
}, { s -> s.unPropertyName.doc() })

// TODO add coverage
// TODO add discards in the shrink case
fun Report<Progress>.prettyProgress(name: Option<PropertyName>): Doc<Markup> = when (status) {
    is Progress.Running -> (bullet().annotate(Markup.Icon(IconType.Running)) spaced
            name.doc() spaced
            "passed".text() spaced
            numTests.testCount() +
            numDiscarded.discardCount() spaced
            "(running)".text()).annotate(Markup.Progress.Running)
    is Progress.Shrinking -> ("↯".text().annotate(Markup.Icon(IconType.Shrinking)) spaced
            name.doc() spaced
            "failed after".text() spaced
            numTests.testCount() spaced
            "(shrinking)".text()).annotate(Markup.Progress.Shrinking)
}

fun Report<Result>.prettyResult(name: Option<PropertyName>): Doc<Markup> = when (status) {
    is Result.Success -> ("✓".text().annotate(Markup.Icon(IconType.Success)) spaced
            name.doc() spaced
            "passed".text() spaced
            numTests.testCount() + dot()).annotate(Markup.Result.Success)
    is Result.GivenUp -> ("⚐".text().annotate(Markup.Icon(IconType.GaveUp)) spaced
            name.doc() spaced
            "gave up after".text() +
            numDiscarded.testCount() + comma() spaced
            "passed".text() spaced
            numTests.testCount() + dot()).annotate(Markup.Result.GaveUp)
    is Result.Failure -> ("\uD83D\uDFAC".text().annotate(Markup.Icon(IconType.Failure)) spaced
            name.doc() spaced
            "failed after".text() spaced
            numTests.testCount() +
            numDiscarded.discardCount() spaced
            status.summary.numShrinks.shrinkCount() + dot()).annotate(Markup.Result.Failed) line
            status.summary.pretty()
}

fun FailureSummary.pretty(): Doc<Markup> = annotations.prettyAnnotations() line
        failureDoc line
        footnotes.map { it.annotate(Markup.Footnote) }.vSep()

fun List<Doc<Markup>>.prettyAnnotations(): Doc<Markup> =
    if (size == 1) ("forAll".text() spaced "=".text() +
            (line() + first().annotate(Markup.Annotation)).nest(2)).group()
    else {
        val szLen = "$size".length
        withIndex().map { (i, v) ->
            ("forAll".text() + (i + 1).doc().fill(szLen) spaced pretty.symbols.equals() +
                    (line() + v.annotate(Markup.Annotation)).nest(2)).group()
        }.vSep()
    }

fun Int.testCount(): Doc<Nothing> = plural("test".text(), "tests".text())

fun Int.shrinkCount(): Doc<Nothing> = plural("shrink".text(), "shrinks".text())

fun Int.discardCount(): Doc<Nothing> =
    if (this == 0) nil()
    else " with".text() spaced this.doc() spaced "discarded".text()

fun <A> Int.plural(singular: Doc<A>, plural: Doc<A>): Doc<A> =
    if (this == 1) doc() spaced singular
    else doc() spaced plural

// rendering
// TODO better colour support
// TODO I need a better algo for diffs that can nest the diff at any nesting
fun Doc<Markup>.render(useColor: UseColor): String = alterAnnotations {
    if (useColor == UseColor.EnableColor) when (it) {
        is Markup.Diff -> emptyList()
        is Markup.DiffAdded -> listOf(
            Style.Prefix("+", it.offset), Style.Ansi(colorDull(Color.Green))
        )
        is Markup.DiffRemoved -> listOf(
            Style.Prefix("-", it.offset), Style.Ansi(colorDull(Color.Red))
        )
        is Markup.Result.Failed -> listOf(Style.Ansi(colorDull(Color.Red)))
        is Markup.Result.Success -> listOf(Style.Ansi(colorDull(Color.Green)))
        is Markup.Progress -> listOf(Style.Ansi(colorDull(Color.Red)))
        is Markup.Annotation -> listOf(Style.Ansi(colorDull(Color.Magenta)))
        else -> emptyList()
    }
    else when (it) {
        is Markup.Diff -> emptyList()
        is Markup.DiffAdded -> listOf(Style.Prefix("+", it.offset))
        is Markup.DiffRemoved -> listOf(Style.Prefix("-", it.offset))
        else -> emptyList()
    }
}
    .layoutPretty(PageWidth.Available(120, 0.5F))
    .renderMarkup()

fun Report<Progress>.renderProgress(useColor: UseColor, name: Option<PropertyName>): String =
    prettyProgress(name).render(useColor)

fun Report<Result>.renderResult(useColor: UseColor, name: Option<PropertyName>): String =
    prettyResult(name).render(useColor)

sealed class Style {
    data class Prefix(val pre: String, val col: Int) : Style()
    data class Ansi(val st: AnsiStyle) : Style()
}

fun SimpleDoc<Style>.renderMarkup(): String {
    tailrec fun SimpleDoc<Style>.go(
        xs: List<Style>,
        cont: (String) -> String
    ): String = when (val dF = unDoc.value()) {
        is SimpleDocF.Fail -> TODO("Better error")
        is SimpleDocF.Nil -> cont("")
        is SimpleDocF.Line -> dF.doc.go(xs, AndThen(cont).compose { str ->
            xs.firstOrNull { it is Style.Prefix }.toOption().fold({
                "\n${spaces(dF.i)}$str"
            }, {
                (it as Style.Prefix)
                "\n${spaces(it.col)}${it.pre}${spaces(dF.i - it.col - 1)}$str"
            })
        })
        is SimpleDocF.Text -> dF.doc.go(xs, AndThen(cont).compose { dF.str + it })
        is SimpleDocF.AddAnnotation -> when (val a = dF.ann) {
            is Style.Prefix -> dF.doc.go(listOf(dF.ann) + xs, cont)
            is Style.Ansi -> {
                val currStyle = xs.firstOrNull { it is Style.Ansi }
                val newS = a.st + (currStyle as Style.Ansi).st
                dF.doc.go(listOf(Style.Ansi(newS)) + xs, AndThen(cont).compose {
                    newS.toRawString() + it
                })
            }
            else -> dF.doc.go(listOf(dF.ann) + xs, cont)
        }
        is SimpleDocF.RemoveAnnotation -> dF.doc.go(xs.tail(), AndThen(cont).compose { str ->
            if (xs.first() is Style.Ansi)
                xs.tail().firstOrNull { it is Style.Ansi }!!.let {
                    (it as Style.Ansi)
                    it.st.toRawString() + str
                }
            else str
        })
    }
    return go(listOf(Style.Ansi(AnsiStyle.monoid().empty())), ::identity)
}
