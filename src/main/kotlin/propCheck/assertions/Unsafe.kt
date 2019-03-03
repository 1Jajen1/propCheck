package propCheck.assertions

import arrow.Kind
import arrow.core.toT
import arrow.typeclasses.Monad
import propCheck.Gen
import propCheck.fix
import propCheck.gen.monad.monad

fun <F, A> Kind<F, Gen<A>>.promote(M: Monad<F>): Gen<Kind<F, A>> = Gen.monad().binding {
    val eval = delay<A>().bind()
    M.lift(eval).invoke(this@promote)
}.fix()

fun <A> delay(): Gen<(Gen<A>) -> A> = Gen { (r, n) ->
    { g: Gen<A> ->
        g.unGen(r toT n)
    }
}