package io.jannis.propTest.instances

import io.jannis.propTest.Arbitrary
import io.jannis.propTest.Gen
import io.jannis.propTest.shrinkList
import arrow.data.Nel
import arrow.extension

@extension
interface NonEmptyListArbitrary<A> : Arbitrary<Nel<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<Nel<A>> = AA().arbitrary().listOf().suchThatMap { Nel.fromList(it) }
    override fun shrink(fail: Nel<A>): Sequence<Nel<A>> = shrinkList<A> { AA().shrink(it) }.invoke(fail.all)
        .filter { it.isNotEmpty() }.map { Nel.fromListUnsafe(it) }
}