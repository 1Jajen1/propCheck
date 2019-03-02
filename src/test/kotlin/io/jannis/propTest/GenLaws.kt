package io.jannis.propTest

import arrow.core.toT
import arrow.test.laws.MonadLaws
import arrow.typeclasses.Eq
import io.jannis.propTest.gen.monad.monad
import io.kotlintest.TestCaseConfig
import io.kotlintest.TestType
import io.kotlintest.specs.AbstractStringSpec
import java.time.Duration

class GenSpec : AbstractStringSpec() {
    init {
        MonadLaws.laws(
            Gen.monad(),
            Eq { a, b ->
                a.fix().unGen(0L toT 10) == b.fix().unGen(0L toT 10)
            }
        ).map {
            this@GenSpec.addTestCase(
                it.name,
                { it.test(this) },
                TestCaseConfig(
                    true,
                    1,
                    Duration.ZERO,
                    1,
                    emptySet(),
                    emptyList()
                ),
                TestType.Test
            )
        }
    }
}