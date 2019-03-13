package propCheck.arbitrary

import io.kotlintest.specs.StringSpec
import propCheck.*
import propCheck.property.testable.testable

class IntegralShrinkSpec : StringSpec({
    "shrinking ints should yield a sorted list of integers" {
        propCheck(Args(maxSuccess = 10000)) {
            forAll(arbitraryBoundedInt(), Property.testable()) { i: Int ->
                shrinkInt(i).take(100).toList().let {
                    counterexample(
                        "$it",
                        it.sorted() == it || it.sorted().reversed() == it
                    )
                }
            }
        }
    }
    "shrinking longs should yield a sorted list of longs" {
        propCheck(Args(maxSuccess = 10000)) {
            forAll(arbitraryBoundedLong(), Property.testable()) { i: Long ->
                shrinkLong(i).take(100).toList().let {
                    counterexample(
                        "$it",
                        it.sorted() == it || it.sorted().reversed() == it
                    )
                }
            }
        }
    }
    "shrinking bytes should yield a sorted list of byte" {
        propCheck(Args(maxSuccess = 10000)) {
            forAll(arbitraryBoundedByte(), Property.testable()) { i: Byte ->
                shrinkByte(i).take(100).toList().let {
                    counterexample(
                        "$it",
                        it.sorted() == it || it.sorted().reversed() == it
                    )
                }
            }
        }
    }
})

class ShrinkListSpec : StringSpec({
    "shrinkList should generate an ordered sequence of smaller lists" {
        propCheck(Args(maxSuccess = 1000)) {
            forAll { l: List<Int> ->
                shrinkList<Int> { shrinkInt(it) }.invoke(l).take(100).toList().let {
                    counterexample(
                        "$it",
                        it.zipWithNext().fold(true) { acc, (l, r) ->
                            acc && (l.size <= r.size)
                        }
                    )
                }
            }
        }
    }
})