package propCheck.instances

import arrow.core.*
import arrow.extension
import arrow.typeclasses.Show
import propCheck.arbitrary.*
import propCheck.arbitrary.gen.applicative.applicative
import propCheck.arbitrary.tuple2.func.func
import propCheck.instances.either.func.func
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
        funMap(Either.func(Either.func(LF(), RF()), Tuple2.func(LF(), RF())), {
            it.fold({ it.left().left() }, { it.right().left() }, { l, r -> (l toT r).right() })
        }, {
            it.fold({
                it.fold({ it.leftIor() }, { it.rightIor() })
            }, { (l, r) -> Ior.Both(l, r) })
        }, f)
}

@extension
interface IorCoarbitrary<L, R> : Coarbitrary<Ior<L, R>> {
    fun CL(): Coarbitrary<L>
    fun CR(): Coarbitrary<R>

    override fun <B> Gen<B>.coarbitrary(a: Ior<L, R>): Gen<B> = a.fold({ l ->
        CL().run { coarbitrary(l).variant(0) }
    }, { r ->
        CR().run { coarbitrary(r).variant(1) }
    }, { l, r ->
        CL().run {
            CR().run {
                coarbitrary(l).coarbitrary(r).variant(2)
            }
        }
    })
}
