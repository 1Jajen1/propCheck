package propCheck.property

import arrow.core.ListK
import arrow.core.Option
import arrow.core.Tuple2
import arrow.core.extensions.listk.monoid.monoid
import arrow.core.toT
import arrow.extension
import arrow.optics.optics
import arrow.typeclasses.Monoid
import arrow.typeclasses.Semigroup
import pretty.Doc
import kotlin.math.ln
import kotlin.math.pow
import kotlin.math.roundToInt
import kotlin.math.sqrt

sealed class Markup {
    data class DiffRemoved(val offset: Int) : Markup()
    data class DiffAdded(val offset: Int) : Markup()
    object Diff : Markup()
    object Annotation : Markup()
    object Footnote : Markup()
    object Coverage : Markup()
    object CoverageFill : Markup()
    sealed class Progress : Markup() {
        object Running : Progress()
        object Shrinking : Progress()
    }
    sealed class Result : Markup() {
        object Failed : Result()
        object GaveUp : Result()
        object Success : Result()
    }
    data class Icon(val name: IconType): Markup()
    sealed class Style : Markup() {
        object Failure : Style()
        object Success : Style()
    }
}

sealed class IconType {
    object GaveUp : IconType()
    object Failure : IconType()
    object Success : IconType()
    object Running : IconType()
    object Shrinking : IconType()
    object Coverage : IconType()
}

// TODO if possible (might be hard within the jvm, but maybe gradle has options) later on we can extend this with source pos info to print text at specific points in test source
inline class Failure(val unFailure: Doc<Markup>)

inline class Log(val unLog: List<JournalEntry>) {
    companion object
}

@extension
interface LogMonoid: Monoid<Log> {
    override fun Log.combine(b: Log): Log = Log(
        ListK.monoid<JournalEntry>().run { unLog + b.unLog }
    )
    override fun empty(): Log = Log(ListK.empty())
}

sealed class JournalEntry {
    // inputs forAll adds those
    data class Input(val text: () -> Doc<Markup>) : JournalEntry()
    // user supplied information
    data class Annotate(val text: () -> Doc<Markup>) : JournalEntry()

    data class Footnote(val text: () -> Doc<Markup>) : JournalEntry()
    // labels, classes, tables
    data class JournalLabel(val label: Label<Boolean>): JournalEntry()
}

data class Label<A>(
    val table: Option<LabelTable>,
    val name: LabelName,
    val min: CoverPercentage,
    val annotation: A
)

inline class CoverPercentage(val unCoverPercentage: Double)

inline class LabelTable(val unLabelTable: String)

inline class LabelName(val unLabelName: String)

data class Coverage<A>(val unCoverage: Map<Option<LabelTable>, Map<LabelName, Label<A>>>) {
    companion object {
        fun <A> monoid(SA: Semigroup<A>) = object: CoverageMonoid<A> {
            override fun SA(): Semigroup<A> = SA
        }
    }
}

fun Coverage<CoverCount>.coverageSuccess(test: TestCount): Boolean =
    coverageFailures(test).isEmpty()

fun Coverage<CoverCount>.coverageFailures(testCount: TestCount): List<Label<CoverCount>> =
    unCoverage.values.flatMap { it.values }.filter { it.labelCovered(testCount).not() }

interface CoverageMonoid<A> : Monoid<Coverage<A>> {
    fun SA(): Semigroup<A>
    override fun empty(): Coverage<A> = Coverage(emptyMap())
    override fun Coverage<A>.combine(b: Coverage<A>): Coverage<A> =
        Coverage(
            b.unCoverage.toList().fold(unCoverage) { acc, (k, v) ->
                if (acc.containsKey(k))
                    acc + mapOf(k to v.toList().fold(acc.getValue(k)) { acc, (k2, v2) ->
                        if (acc.containsKey(k2))
                            acc + mapOf(k2 to (Label(v2.table, v2.name, v2.min, SA().run { acc[k2]!!.annotation + v2.annotation })))
                        else acc + mapOf(k2 to v2)
                    })
                else acc + mapOf(k to v)
            }
        )
}

data class CoverCount(val unCoverCount: Int) {
    companion object {
        fun semigroup() = object : CoverCountSemigroup {}
    }
}

interface CoverCountSemigroup : Semigroup<CoverCount> {
    override fun CoverCount.combine(b: CoverCount): CoverCount =
        CoverCount(unCoverCount + b.unCoverCount)
}

fun CoverCount.coverPercentage(test: TestCount): CoverPercentage =
    CoverPercentage(((unCoverCount.toDouble() / test.unTestCount.toDouble() * 100.0) * 10).roundToInt().toDouble() / 10.0)

fun Label<CoverCount>.labelCovered(test: TestCount): Boolean =
    annotation.coverPercentage(test).unCoverPercentage >= min.unCoverPercentage

@optics
data class PropertyConfig(
    val terminationCriteria: TerminationCriteria = NoConfidenceTermination(),
    val maxDiscardRatio: DiscardRatio = DiscardRatio(10.0),
    val shrinkLimit: ShrinkLimit = ShrinkLimit(1000),
    val shrinkRetries: ShrinkRetries = ShrinkRetries(0)
) {
    companion object
}

val defaultMinTests = TestLimit(100)

data class Confidence(val certainty: Long = 10.0.pow(9.0).toLong(), val tolerance: Double = 0.9)

sealed class TerminationCriteria
data class EarlyTermination(val confidence: Confidence = Confidence(), val limit: Int = 100): TerminationCriteria()
data class NoEarlyTermination(val confidence: Confidence = Confidence(), val limit: Int = 100): TerminationCriteria()
data class NoConfidenceTermination(val limit: Int = 100): TerminationCriteria()

inline class TestLimit(val unTestLimit: Int)
inline class DiscardRatio(val unDiscardRatio: Double)
inline class ShrinkLimit(val unShrinkLimit: Int)
inline class ShrinkRetries(val unShrinkRetries: Int)

inline class TestCount(val unTestCount: Int)
inline class ShrinkCount(val unShrinkCount: Int)
inline class DiscardCount(val unDiscardCount: Int)

inline class PropertyName(val unPropertyName: String)
inline class GroupName(val unGroupName: String)

inline class Size(val unSize: Int)

fun Coverage<CoverCount>.labelsToTotals(): List<Tuple2<Int, Label<CoverCount>>> =
    unCoverage.values.flatMap {
        val total = it.values.sumBy { it.annotation.unCoverCount }
        it.values.map { total toT it }
    }

fun Confidence.success(test: TestCount, coverage: Coverage<CoverCount>): Boolean =
    coverage.labelsToTotals().map { (total, l) ->
        sufficientlyCovered(l.table.fold({ test.unTestCount }, { total }), l.annotation.unCoverCount, l.min.unCoverPercentage / 100.0)
    }.fold(true) { acc, v -> acc && v }

fun Confidence.failure(test: TestCount, coverage: Coverage<CoverCount>): Boolean =
    coverage.labelsToTotals().map { (total, l) ->
        insufficientlyCovered(l.table.fold({ test.unTestCount }, { total }), l.annotation.unCoverCount, l.min.unCoverPercentage / 100.0)
    }.fold(false) { acc, v -> acc || v }

fun Confidence.sufficientlyCovered(n: Int, k: Int, p: Double): Boolean =
    wilsonLow(k, n, 1.toDouble() / certainty) >= tolerance * p

fun Confidence.insufficientlyCovered(n: Int, k: Int, p: Double): Boolean =
    wilsonHigh(k, n, 1.toDouble() / certainty) < p

fun wilsonLow(k: Int, n: Int, a: Double): Double = wilson(k, n, invnormcdf(a / 2))

fun wilsonHigh(k: Int, n: Int, a: Double): Double = wilson(k, n, invnormcdf(1 - a / 2))

fun wilson(k: Int, n: Int, z: Double): Double {
    val p = k / n.toDouble()
    return (p + z * z / (2 * n) + z * Math.sqrt(p * (1 - p) / n + z * z / (4 * n * n))) / (1 + z * z / n)
}


// Quickcheck added this without erfc, so I'll use that. Credits to them:
//  https://github.com/nick8325/quickcheck/blob/master/Test/QuickCheck/Test.hs
//  and the source quickcheck used https://web.archive.org/web/20151110174102/http://home.online.no/~pjacklam/notes/invnorm/
fun invnormcdf(d: Double): Double = when {
    d > 1 -> Double.NaN
    d < 0 -> Double.NaN
    d == 0.0 -> Double.NEGATIVE_INFINITY
    d == 1.0 -> Double.POSITIVE_INFINITY
    else -> {
        val dLow = 0.02425
        val dHigh = 1 - dLow

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

        when {
            d < dLow -> {
                val q = sqrt(-2 * ln(d))
                (((((c1 * q + c2) * q + c3) * q + c4) * q + c5) * q + c6) /
                        ((((d1 * q + d2) * q + d3) * q + d4) * q + 1)
            }
            d <= dHigh -> {
                val q = d - 0.5
                val r = q * q
                (((((a1 * r + a2) * r + a3) * r + a4) * r + a5) * r + a6) * q /
                        (((((b1 * r + b2) * r + b3) * r + b4) * r + b5) * r + 1)
            }
            else -> {
                val q = sqrt(ln(1 - d) * -2)
                -(((((c1 * q + c2) * q + c3) * q + c4) * q + c5) * q + c6) /
                        ((((d1 * q + d2) * q + d3) * q + d4) * q + 1)
            }
        }
    }
}