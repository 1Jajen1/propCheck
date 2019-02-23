package io.jannis.propTest.instances

import io.jannis.propTest.Arbitrary
import io.jannis.propTest.Gen
import arrow.core.Either
import arrow.core.left
import arrow.core.right
import arrow.extension

@extension
interface EitherArbitrary<L, R> : Arbitrary<Either<L, R>> {
    fun AL(): Arbitrary<L>
    fun AR(): Arbitrary<R>

    override fun arbitrary(): Gen<Either<L, R>> = Gen.oneOf<Either<L, R>>(
        AL().arbitrary().map { it.left() },
        AR().arbitrary().map { it.right() }
    )

    override fun shrink(fail: Either<L, R>): Sequence<Either<L, R>> = fail.fold({ l ->
        AL().shrink(l).map { it.left() }
    }, { r ->
        AR().shrink(r).map { it.right() }
    })
}