package propCheck.property

import arrow.core.ListK
import arrow.core.Option
import arrow.core.Tuple2
import arrow.core.extensions.listk.monoid.monoid
import arrow.extension
import arrow.optics.optics
import arrow.typeclasses.Monoid
import arrow.typeclasses.Semigroup
import pretty.Doc

sealed class Markup {
    data class DiffRemoved(val offset: Int) : Markup()
    data class DiffAdded(val offset: Int) : Markup()
    object Diff : Markup()
    object Annotation : Markup()
    object Footnote : Markup()
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

inline class Coverage<A>(val unCoverage: Map<Option<LabelTable>, Map<LabelName, Label<A>>>) {
    companion object
}

@extension
interface CoverageMonoid<A> : Monoid<Coverage<A>> {
    fun SA(): Semigroup<A>
    override fun empty(): Coverage<A> = Coverage(emptyMap())
    override fun Coverage<A>.combine(b: Coverage<A>): Coverage<A> =
        // TODO this is ugly
        Coverage(
            b.unCoverage.toList().fold(unCoverage) { acc, (k, v) ->
                if (acc.containsKey(k))
                    acc + mapOf(k to v.toList().fold(acc[k]!!) { acc, (k2, v2) ->
                        if (acc.containsKey(k2))
                            acc + mapOf(k2 to (Label(v2.table, v2.name, v2.min, SA().run { acc[k2]!!.annotation + v2.annotation })))
                        else acc + mapOf(k2 to v2)
                    })
                else acc + mapOf(k to v)
            }
        )
}

inline class CoverCount(val unCoverCount: Int) {
    companion object {
        fun semigroup() = object : CoverCountSemigroup {}
    }
}

interface CoverCountSemigroup : Semigroup<CoverCount> {
    override fun CoverCount.combine(b: CoverCount): CoverCount =
        CoverCount(unCoverCount + b.unCoverCount)
}

@optics
data class PropertyConfig(
    val testLimit: TestLimit = TestLimit(100),
    val maxDiscardRatio: DiscardRatio = DiscardRatio(10.0),
    val shrinkLimit: ShrinkLimit = ShrinkLimit(1000),
    val shrinkRetries: ShrinkRetries = ShrinkRetries(0)
) {
    companion object
}

inline class TestLimit(val unTestLimit: Int)
inline class DiscardRatio(val unDiscardRatio: Double)
inline class ShrinkLimit(val unShrinkLimit: Int)
inline class ShrinkRetries(val unShrinkRetries: Int)

inline class PropertyName(val unPropertyName: String)
inline class GroupName(val unGroupName: String)

inline class Size(val unSize: Int)

// TODO
data class Group(
    val name: GroupName,
    val props: List<Tuple2<PropertyName, Property>>
)