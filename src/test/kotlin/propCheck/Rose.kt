package propCheck

import arrow.Kind
import arrow.test.laws.MonadLaws
import arrow.typeclasses.Eq
import propCheck.rose.monad.monad

class RoseSpec : LawSpec() {
    fun roseEq(): Eq<Kind<ForRose, Int>> = Eq { a, b ->
        when (val aF = a.fix()) {
            is Rose.IORose -> when (val bF = b.fix()) {
                is Rose.IORose -> roseEq().run { aF.ioRose.unsafeRunSync().eqv(bF.ioRose.unsafeRunSync()) }
                is Rose.MkRose -> false
            }
            is Rose.MkRose -> when (val bF = b.fix()) {
                is Rose.MkRose -> aF.res == bF.res && roseEq().run {
                    aF.shrunk.zip(bF.shrunk).fold(true) { acc, (a, b) ->
                        acc && a.eqv(b)
                    }
                }
                is Rose.IORose -> false
            }
        }
    }

    init {
        testLaws(
            MonadLaws.laws(Rose.monad(), roseEq())
        )
    }
}