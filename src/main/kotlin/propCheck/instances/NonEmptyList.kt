package propCheck.instances

import arrow.data.Nel
import arrow.data.NonEmptyList
import arrow.extension
import propCheck.Arbitrary
import propCheck.Gen
import propCheck.shrinkList

@extension
interface NonEmptyListArbitrary<A> : Arbitrary<NonEmptyList<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<Nel<A>> = AA().arbitrary().listOf().suchThatMap { Nel.fromList(it) }
    override fun shrink(fail: Nel<A>): Sequence<Nel<A>> = shrinkList<A> { AA().shrink(it) }.invoke(fail.all)
        .filter { it.isNotEmpty() }.map { Nel.fromListUnsafe(it) }
}