package io.jannis.propTest.instances

import io.jannis.propTest.Arbitrary
import io.jannis.propTest.Gen
import arrow.extension
import arrow.typeclasses.Const
import arrow.typeclasses.const

@extension
interface ConstArbitrary<A, T> : Arbitrary<Const<A, T>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<Const<A, T>> = AA().arbitrary().map { it.const() }
    override fun shrink(fail: Const<A, T>): Sequence<Const<A, T>> = AA().shrink(fail.value())
        .map { it.const() }
}