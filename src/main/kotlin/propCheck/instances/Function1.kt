package propCheck.instances

import arrow.core.Function1
import arrow.core.extensions.function1.monad.monad
import arrow.core.fix
import arrow.extension
import propCheck.arbitrary.Arbitrary
import propCheck.arbitrary.Coarbitrary
import propCheck.arbitrary.Gen
import propCheck.promote

@extension
interface Function1Arbitrary<A, B> : Arbitrary<Function1<A, B>> {
    fun CA(): Coarbitrary<A>
    fun AB(): Arbitrary<B>
    override fun arbitrary(): Gen<Function1<A, B>> =
        Function1 { a: A -> CA().run { AB().arbitrary().coarbitrary(a) } }.promote(Function1.monad()).map { it.fix() }
}