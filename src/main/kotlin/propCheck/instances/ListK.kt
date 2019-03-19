package propCheck.instances

import arrow.data.ListK
import arrow.data.k
import arrow.extension
import arrow.typeclasses.Show
import propCheck.arbitrary.Arbitrary
import propCheck.arbitrary.Gen
import propCheck.arbitrary.shrinkList
import propCheck.arbitrary.shrinkMap

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
    override fun shrink(fail: List<A>): Sequence<List<A>> = shrinkList(fail) { AA().shrink(it) }
}

interface ListKShow<A> : Show<ListK<A>> {
    fun SA(): Show<A>
    override fun ListK<A>.show(): String =
            "List(" + joinToString { SA().run { it.show() } } + ")"
}


interface ListShow<A> : Show<List<A>> {
    fun SA(): Show<A>
    override fun List<A>.show(): String =
        "List(" + joinToString { SA().run { it.show() } } + ")"
}