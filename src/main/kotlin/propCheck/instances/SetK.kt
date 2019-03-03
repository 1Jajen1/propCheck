package propCheck.instances

import arrow.data.SetK
import arrow.data.k
import arrow.extension
import propCheck.Arbitrary
import propCheck.Gen
import propCheck.shrinkList

@extension
interface SetKArbitrary<A> : Arbitrary<SetK<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<SetK<A>> = AA().arbitrary().listOf().map { it.toSet().k() }
    override fun shrink(fail: SetK<A>): Sequence<SetK<A>> = shrinkList<A> { AA().shrink(it) }
        .invoke(fail.toList()).map { it.toSet().k() }
}