package propCheck.instances

import arrow.core.Id
import arrow.core.value
import arrow.extension
import arrow.typeclasses.Show
import propCheck.Arbitrary
import propCheck.Gen

@extension
interface IdArbitrary<A> : Arbitrary<Id<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<Id<A>> = AA().arbitrary().map { Id.just(it) }
    override fun shrink(fail: Id<A>): Sequence<Id<A>> = AA().shrink(fail.value()).map { Id.just(it) }
}

interface IdShow<A> : Show<Id<A>> {
    fun SA(): Show<A>
    override fun Id<A>.show(): String =
            "Id(" + SA().run { value().show() } + ")"
}