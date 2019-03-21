package propCheck.instances

import arrow.data.Ior
import arrow.data.leftIor
import arrow.data.rightIor
import arrow.extension
import arrow.typeclasses.Show
import propCheck.arbitrary.Arbitrary
import propCheck.arbitrary.Gen
import propCheck.arbitrary.fix
import propCheck.arbitrary.gen.applicative.applicative

@extension
interface IorArbitrary<L, R> : Arbitrary<Ior<L, R>> {
    fun AL(): Arbitrary<L>
    fun AR(): Arbitrary<R>
    override fun arbitrary(): Gen<Ior<L, R>> = Gen.oneOf(
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

interface IorShow<L, R> : Show<Ior<L, R>> {
    fun SL(): Show<L>
    fun SR(): Show<R>
    override fun Ior<L, R>.show(): String = fold({
        "Left(" + SL().run { it.show() } + ")"
    }, {
        "Right(" + SR().run { it.show() } + ")"
    }, { l, r ->
        "Both(" + SL().run { l.show() } + "," + SR().run { r.show() } + ")"
    })
}