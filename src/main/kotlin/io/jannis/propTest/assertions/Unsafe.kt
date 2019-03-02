package io.jannis.propTest.assertions

import arrow.Kind
import arrow.core.toT
import arrow.typeclasses.Monad
import io.jannis.propTest.Gen
import io.jannis.propTest.fix
import io.jannis.propTest.gen.monad.monad

fun <F, A> Kind<F, Gen<A>>.promote(M: Monad<F>): Gen<Kind<F, A>> = Gen.monad().binding {
    val eval = delay<A>().bind()
    M.lift(eval).invoke(this@promote)
}.fix()

fun <A> delay(): Gen<(Gen<A>) -> A> = Gen { (r, n) ->
    { g: Gen<A> ->
        g.unGen(r toT n)
    }
}