package propCheck.instances

import arrow.data.Validated
import arrow.data.invalid
import arrow.data.valid
import arrow.extension
import arrow.typeclasses.Show
import propCheck.arbitrary.Arbitrary
import propCheck.arbitrary.Gen

@extension
interface ValidatedArbitrary<E, A> : Arbitrary<Validated<E, A>> {
    fun AE(): Arbitrary<E>
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<Validated<E, A>> = Gen.oneOf(
        AE().arbitrary().map { it.invalid() },
        AA().arbitrary().map { it.valid() }
    )

    override fun shrink(fail: Validated<E, A>): Sequence<Validated<E, A>> = fail.fold({
        AE().shrink(it).map { it.invalid() }
    }, {
        AA().shrink(it).map { it.valid() }
    })
}

interface ValidatedShow<E, A> : Show<Validated<E, A>> {
    fun SE(): Show<E>
    fun SA(): Show<A>
    override fun Validated<E, A>.show(): String = fold({
        "Invalid(" + SE().run { it.show() } + ")"
    }, {
        "Valid(" + SA().run { it.show() } + ")"
    })
}