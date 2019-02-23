package io.jannis.propTest.instances

import io.jannis.propTest.Arbitrary
import io.jannis.propTest.Gen
import io.jannis.propTest.shrinkList
import io.jannis.propTest.shrinkMap
import arrow.data.ListK
import arrow.data.k
import arrow.extension

@extension
interface ListKArbitrary<A> : Arbitrary<ListK<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<ListK<A>> = AA().arbitrary().listOf().map { it.k() }
    override fun shrink(fail: ListK<A>): Sequence<ListK<A>> = shrinkMap({
        it
    }, {
        it.k()
    }, object : ListArbitrary<A> {
        override fun AA(): Arbitrary<A> = this@ListKArbitrary.AA()
    }).invoke(fail)
}

interface ListArbitrary<A> : Arbitrary<List<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<List<A>> = AA().arbitrary().listOf()
    override fun shrink(fail: List<A>): Sequence<List<A>> = shrinkList<A> { AA().shrink(it) }
        .invoke(fail)
}