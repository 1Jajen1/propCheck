package propCheck.arbitrary

import arrow.core.toT
import pretty.text
import propCheck.property.EarlyTermination
import propCheck.property.PropertyConfig
import propCheck.property.Size
import propCheck.property.property
import propCheck.util.PropertySpec

class RangeTest : PropertySpec({
    "Ranges"(listOf(
        "Range.constant should generate values in the correct range" toT property(PropertyConfig(terminationCriteria = EarlyTermination())) {
            val sz = forAll { int(0..100) }.bind()
            val (lower, upper) = forAll {
                int(Range.constant(0, Int.MIN_VALUE, Int.MAX_VALUE)).let {
                    tupledN(
                        it,
                        it
                    )
                }
            }.bind()

            if (lower > upper) discard<Unit>().bind()

            val range = Range.constant(lower, upper)
            val (l, u) = range.bounds(Size(sz))

            // verify that both edge cases happen
            cover(0.5, "Size = 0", sz == 0).bind()
            cover(0.5, "Size = 99", sz == 99).bind()

            annotate { "Lower bound $l".text() }.bind()
            annotate { "Upper bound $u".text() }.bind()

            if (lower > l) failWith("Lower bound out of range").bind()
            if (upper <= l) failWith("Upper bound out of range").bind()
        },
        "Range.linear should increase the bounds and stay in bounds" toT property(PropertyConfig(terminationCriteria = EarlyTermination())) {
            val sz = forAll { int(0..100) }.bind()
            val (lower, upper) = forAll {
                int(Range.constant(0, Int.MIN_VALUE, Int.MAX_VALUE)).let {
                    tupledN(
                        it,
                        it
                    )
                }
            }.bind()

            if (lower > upper) discard<Unit>().bind()

            val range = Range.linear(lower, upper)
            val (l, u) = range.bounds(Size(sz))

            // verify that both edge cases happen
            cover(0.5, "Size = 0", sz == 0).bind()
            cover(0.5, "Size = 99", sz == 99).bind()

            annotate { "Lower bound $l".text() }.bind()
            annotate { "Upper bound $u".text() }.bind()

            if (lower > l) failWith("Lower bound out of range").bind()
            if (upper <= l) failWith("Upper bound out of range").bind()

            if (sz < 99) {
                val (l1, u1) = range.bounds(Size(sz + 1))
                if (l1 > l) failWith("Lower bound got larger!").bind()
                if (u1 < u) failWith("Upper bound got smaller").bind()
            }
        }
    ))
})
