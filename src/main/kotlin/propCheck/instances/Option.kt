package propCheck.instances

import arrow.core.Option
import arrow.core.none
import arrow.core.some
import arrow.core.toT
import arrow.extension
import propCheck.Arbitrary
import propCheck.Gen

@extension
interface OptionArbitrary<A> : Arbitrary<Option<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<Option<A>> = Gen.frequency(
        3 toT AA().arbitrary().map { it.some() },
        1 toT Gen.elements(none())
    )

    override fun shrink(fail: Option<A>): Sequence<Option<A>> = fail.fold({
        emptySequence()
    }, { a ->
        sequenceOf<Option<A>>(none()) + AA().shrink(a).map { it.some() }
    })
}