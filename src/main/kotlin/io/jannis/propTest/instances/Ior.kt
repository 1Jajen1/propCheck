package io.jannis.propTest.instances

import io.jannis.propTest.Arbitrary
import io.jannis.propTest.Gen
import arrow.data.Ior
import arrow.data.leftIor
import arrow.data.rightIor
import arrow.extension
import io.jannis.propTest.fix
import io.jannis.propTest.gen.applicative.applicative

@extension
interface IorArbitrary<L, R> : Arbitrary<Ior<L, R>> {
    fun AL(): Arbitrary<L>
    fun AR(): Arbitrary<R>
    override fun arbitrary(): Gen<Ior<L, R>> = Gen.oneOf<Ior<L, R>>(
        AL().arbitrary().map { it.leftIor() },
        AR().arbitrary().map { it.rightIor() },
        Gen.applicative().map(AL().arbitrary(), AR().arbitrary()) { (l, r) ->
            Ior.Both(l, r)
        }.fix()
    )

    override fun shrink(fail: Ior<L, R>): Sequence<Ior<L, R>> = fail.fold({
        AL().shrink(it).map { it.leftIor() }
    }, {
        AR().shrink(it).map { it.rightIor() }
    }, { l, r ->
        AL().shrink(l).map { Ior.Both(it, r) } +
                AR().shrink(r).map { Ior.Both(l, it) }
    })
}