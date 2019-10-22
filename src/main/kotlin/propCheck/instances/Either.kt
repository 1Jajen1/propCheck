package propCheck.instances

import arrow.core.Either
import arrow.core.left
import arrow.core.right
import arrow.extension
import arrow.typeclasses.Show
import propCheck.arbitrary.*

@extension
interface EitherArbitrary<L, R> : Arbitrary<Either<L, R>> {
    fun AL(): Arbitrary<L>
    fun AR(): Arbitrary<R>

    override fun arbitrary(): Gen<Either<L, R>> = Gen.oneOf(
        AL().arbitrary().map { it.left() },
        AR().arbitrary().map { it.right() }
    )

    override fun shrink(fail: Either<L, R>): Sequence<Either<L, R>> = fail.fold({ l ->
        AL().shrink(l).map { it.left() }
    }, { r ->
        AR().shrink(r).map { it.right() }
    })
}

interface EitherShow<L, R> : Show<Either<L, R>> {
    fun SL(): Show<L>
    fun SR(): Show<R>
    override fun Either<L, R>.show(): String = when (this) {
        is Either.Left -> "Left(" + SL().run { a.show() } + ")"
        is Either.Right -> "Right(" + SR().run { b.show() } + ")"
    }
}

@extension
interface EitherFunc<L, R> : Func<Either<L, R>> {
    fun LF(): Func<L>
    fun RF(): Func<R>

    override fun <B> function(f: (Either<L, R>) -> B): Fn<Either<L, R>, B> = funEither(LF(), RF(), f)
}
