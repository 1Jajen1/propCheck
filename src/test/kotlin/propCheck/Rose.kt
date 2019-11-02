package propCheck

import arrow.Kind
import arrow.fx.ForIO
import arrow.fx.IO
import arrow.fx.extensions.io.monad.monad
import arrow.fx.fix
import arrow.test.laws.MonadLaws
import arrow.test.laws.equalUnderTheLaw
import arrow.typeclasses.Eq
import propCheck.property.Rose
import propCheck.property.RosePartialOf
import propCheck.property.fix
import propCheck.property.rose.monad.monad

class RoseSpec : LawSpec() {
    fun roseEq(): Eq<Kind<RosePartialOf<ForIO>, Int>> = Eq { a, b ->
        a.fix().runRose.fix().unsafeRunSync().equalUnderTheLaw(b.fix().runRose.fix().unsafeRunSync(), Eq { a, b ->
            a.res == b.res // shrinking is off for this atm
        })
    }

    init {
        testLaws(
            MonadLaws.laws(Rose.monad(IO.monad()), roseEq())
        )
    }
}