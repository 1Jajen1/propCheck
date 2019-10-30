package propCheck

import arrow.test.laws.Law
import io.kotlintest.TestCaseConfig
import io.kotlintest.TestType
import io.kotlintest.specs.AbstractStringSpec
import io.kotlintest.specs.IntelliMarker
import java.time.Duration

abstract class LawSpec : AbstractStringSpec(), IntelliMarker {
    fun testLaws(laws: List<Law>) {
        laws.map {
            this@LawSpec.addTestCase(
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