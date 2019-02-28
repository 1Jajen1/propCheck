package io.jannis.propTest

import io.jannis.propTest.assertions.*
import io.jannis.propTest.assertions.property.testable.testable
import io.jannis.propTest.instances.arbitrary

fun main() {
    propCheck {
        verbose(Property.testable())(
            forAllShrinkShow(
                Boolean.testable(),
                arbitraryBoundedInt(),
                { Int.arbitrary().shrink(it) },
                { "$it" }).invoke {
                it < 10
            }
        )
    }.unsafeRunSync().also {
        when (it) {
            is Result.Success -> it.output
            is Result.Failure -> it.output
            is Result.GivenUp -> it.output
            is Result.NoExpectedFailure -> it.output
        }.also(::println)
    }
}