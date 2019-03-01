package io.jannis.propTest

import io.jannis.propTest.assertions.*
import io.jannis.propTest.instances.arbitrary

fun main() {
    propCheck {
        forAllShrinkShowP(
            arbitrarySizedInt(),
            { Int.arbitrary().shrink(it) },
            { "$it" }).invoke {
            classifyP(it != 0, "not 0").invoke(
                classify(it == 1, "exactly 1").invoke(
                    it < 1000
                )
            )
        }
    }.unsafeRunSync().also {
        when (it) {
            is Result.Success -> it.output
            is Result.Failure -> it.output
            is Result.GivenUp -> it.output
            is Result.NoExpectedFailure -> it.output
        }.also(::println)
    }
}