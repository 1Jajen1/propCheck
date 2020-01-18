package propCheck.property

import arrow.core.ListK
import arrow.core.Tuple2
import arrow.core.extensions.listk.monoid.monoid
import arrow.extension
import arrow.optics.optics
import arrow.typeclasses.Monoid
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
    // TODO
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

data class Group(
    val name: GroupName,
    val props: List<Tuple2<PropertyName, Property>>
)