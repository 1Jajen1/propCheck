package propCheck

import arrow.Kind
import arrow.core.toT
import arrow.typeclasses.Monad
import propCheck.arbitrary.Gen
import propCheck.arbitrary.fix
import propCheck.arbitrary.gen.monad.monad

fun <F, A> Kind<F, Gen<A>>.promote(M: Monad<F>): Gen<Kind<F, A>> = Gen.monad().fx.monad {
    val eval = delay<A>().bind()
    M.lift(eval).invoke(this@promote)
}.fix()

fun <A> delay(): Gen<(Gen<A>) -> A> = Gen { (r, n) ->
    { g: Gen<A> ->
        g.unGen(r toT n)
    }
}