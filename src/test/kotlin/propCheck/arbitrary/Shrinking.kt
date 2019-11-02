package propCheck.arbitrary

import propCheck.*
import propCheck.property.Property
import propCheck.property.counterexample
import propCheck.property.forAll
import propCheck.property.testable.testable

class IntegralShrinkSpec : PropertySpec({
    "shrinking ints should yield a sorted list of integers"(Args(maxSuccess = 10000)) {
        forAll(arbitraryBoundedInt(), Property.testable()) { i: Int ->
            shrinkInt(i).take(100).toList().let {
                counterexample(
                    { "$it" },
                    it.sorted() == it || it.sorted().reversed() == it
                )
            }
        }
    }
    "shrinking longs should yield a sorted list of longs"(Args(maxSuccess = 10000)) {
        forAll(arbitraryBoundedLong(), Property.testable()) { i: Long ->
            shrinkLong(i).take(100).toList().let {
                counterexample(
                    { "$it" },
                    it.sorted() == it || it.sorted().reversed() == it
                )
            }
        }
    }
    "shrinking bytes should yield a sorted list of byte"(Args(maxSuccess = 10000)) {
        forAll(arbitraryBoundedByte(), Property.testable()) { i: Byte ->
            shrinkByte(i).take(100).toList().let {
                counterexample(
                    { "$it" },
                    it.sorted() == it || it.sorted().reversed() == it
                )
            }
        }
    }
})

class ShrinkListSpec : PropertySpec({
    "shrinkList should generate an ordered sequence of smaller lists"(Args(maxSuccess = 1000)) {
        forAll { l: List<Int> ->
            shrinkList(l) { shrinkInt(it) }.take(100).toList().let {
                counterexample(
                    { "$it" },
                    it.zipWithNext().fold(true) { acc, (l, r) ->
                        acc && (l.size <= r.size)
                    }
                )
            }
        }
    }
})