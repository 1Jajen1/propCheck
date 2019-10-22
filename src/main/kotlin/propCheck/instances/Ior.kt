package propCheck.instances

import arrow.core.*
import arrow.extension
import arrow.typeclasses.Show
import propCheck.arbitrary.*
import propCheck.arbitrary.gen.applicative.applicative
import propCheck.arbitrary.tuple2.func.func
import propCheck.instances.option.func.func
import java.lang.IllegalStateException

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

@extension
interface IorFunc<L, R> : Func<Ior<L, R>> {
    fun LF(): Func<L>
    fun RF(): Func<R>

    override fun <B> function(f: (Ior<L, R>) -> B): Fn<Ior<L, R>, B> =
        funMap(Tuple2.func(Option.func(LF()), Option.func(RF())), {
            it.fold({ it.some() toT none() }, { none<L>() toT it.some() }, { l, r -> l.some() toT r.some() })
        }, { (l, r) ->
            when (l) {
                is Some -> if (r is Some) Ior.Both(l.t, r.t) else Ior.Left(l.t)
                else -> when (r) {
                    is Some -> Ior.Right(r.t)
                    else -> throw IllegalStateException("Cannot and should not happen")
                }
            }
        }, f)
}
