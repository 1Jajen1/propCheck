package propCheck.instances

import arrow.data.ListK
import arrow.data.k
import arrow.extension
import propCheck.Arbitrary
import propCheck.Gen
import propCheck.shrinkList
import propCheck.shrinkMap

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