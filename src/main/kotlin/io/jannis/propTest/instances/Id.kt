package io.jannis.propTest.instances

import io.jannis.propTest.Arbitrary
import io.jannis.propTest.Gen
import arrow.core.Id
import arrow.core.value
import arrow.extension

@extension
interface IdArbitrary<A> : Arbitrary<Id<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<Id<A>> = AA().arbitrary().map { Id.just(it) }
    override fun shrink(fail: Id<A>): Sequence<Id<A>> = AA().shrink(fail.value()).map { Id.just(it) }
}