package propCheck.property

import arrow.core.ListK
import arrow.optics.optics
import pretty.Doc

sealed class Markup {

}

// TODO if possible (might be hard within the jvm, but maybe gradle has options) later on we can extend this with source pos info to print text at specific points in test source
data class Failure(val message: Doc<Markup>)

typealias Log = ListK<JournalEntry>

sealed class JournalEntry {
    // user supplied information
    data class Annotate(val text: () -> Doc<Markup>) : JournalEntry()

    data class Footnote(val text: () -> Doc<Markup>) : JournalEntry()
    // labels, classes, tables
    // TODO
}

// TODO inline classes here!
@optics
data class PropertyConfig(
    val testLimit: Int = 100,
    val maxDiscardRatio: Double = 10.0,
    val shrinkLimit: Int = 1000,
    val shrinkRetries: Int = 0
) {
    companion object
}