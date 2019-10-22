package propCheck.instances

import arrow.core.Either
import arrow.core.Validated
import arrow.core.invalid
import arrow.core.valid
import arrow.extension
import arrow.typeclasses.Show
import propCheck.arbitrary.*
import propCheck.instances.either.func.func

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

@extension
interface ValidatedFunc<E, A> : Func<Validated<E, A>> {
    fun EF(): Func<E>
    fun AF(): Func<A>

    override fun <B> function(f: (Validated<E, A>) -> B): Fn<Validated<E, A>, B> =
        funMap(Either.func(EF(), AF()), {
            it.toEither()
        }, {
            it.fold({ it.invalid() }, { it.valid() })
        }, f)
}
