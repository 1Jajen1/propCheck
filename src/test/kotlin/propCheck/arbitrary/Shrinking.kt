package propCheck.arbitrary

import arrow.core.toT
import pretty.spaced
import pretty.text
import propCheck.pretty.showPretty
import propCheck.property.property
import propCheck.util.PropertySpec

class ShrinkingSpec : PropertySpec({
    "Shrinking"(listOf(
        "Long.shrinktowards should produce an ordered list of values approaching the target" toT property {
            val (l, target) = forAll {
                long(Range.constant(0L, Long.MIN_VALUE, Long.MAX_VALUE)).let {
                    tupledN(
                        it,
                        it
                    )
                }
            }.bind()

            // only checking first 100 because all would take too long
            val first100 = l.shrinkTowards(target).take(100).toList()

            annotate { "First 100:".text() spaced first100.showPretty() }.bind()

            // order
            if (l > target) {
                val ordered = first100.zipWithNext().filter { (a, b) -> a >= b }
                if (ordered.isNotEmpty()) {
                    footnote { "Unordered: Decreasing expected".text() }.bind()
                    val (f, s) = ordered.unzip()
                    diff(f, s) { _, _ -> false }.bind()
                }
            } else if (l == target) {
                if (first100.isNotEmpty()) failWith("Expected no shrunk values, but got some!").bind()
            } else {
                val ordered = first100.zipWithNext().filter { (a, b) -> a <= b }
                if (ordered.isNotEmpty()) {
                    footnote { "Unordered: Increasing expected".text() }.bind()
                    val (f, s) = ordered.unzip()
                    diff(f, s) { _, _ -> false }.bind()
                }
            }
        },
        // TODO this fails because of floating point precision. Is that bad? Should that be solved?
        /*
        "Double.shrinktowards should produce an ordered list of values approaching the target" {
            val (l, target) = forAll { double(Range.constant(0.0, Double.MIN_VALUE, Double.MAX_VALUE)).let { tupledN(it, it) } }.bind()

            // only checking first 100 because all would take too long
            val first100 = l.shrinkTowards(target).take(100).toList()

            annotate { "First 100:".text() spaced first100.showPretty() }.bind()

            // order
            if (l > target) {
                val ordered = first100.zipWithNext().filter { (a, b) -> a > b }
                if (ordered.isNotEmpty()) {
                    footnote { "Unordered: Decreasing expected".text() }.bind()
                    val (f, s) = ordered.unzip()
                    diff(f, s) { _, _ -> false }.bind()
                }
            } else if (l == target) {
                if (first100.isNotEmpty()) failWith("Expected no shrunk values, but got some!").bind()
            } else {
                val ordered = first100.zipWithNext().filter { (a, b) -> a < b }
                if (ordered.isNotEmpty()) {
                    footnote { "Unordered: Increasing expected".text() }.bind()
                    val (f, s) = ordered.unzip()
                    diff(f, s) { _, _ -> false }.bind()
                }
            }
        }
         */
        "List.shrink should produce a sequence of smaller or equal sized lists" toT property {
            val l = forAll { int(-100..100).list(0..100) }.bind()

            // only checking first 100 because all would take too long
            val first100 = l.shrink().take(100).toList()

            annotate { "First 100:".text() spaced first100.showPretty() }.bind()

            // order
            val ordered = first100.zipWithNext().filter { (a, b) -> a.size > b.size }
            if (ordered.isNotEmpty()) {
                val (f, s) = ordered.unzip()
                diff(f, s) { _, _ -> false }.bind()
            }
        }
    ))
})