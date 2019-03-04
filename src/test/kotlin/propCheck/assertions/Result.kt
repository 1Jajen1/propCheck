package propCheck.assertions

import arrow.core.Tuple2
import arrow.core.some
import arrow.core.toT
import arrow.effects.IO
import arrow.effects.extensions.io.applicativeError.attempt
import arrow.effects.extensions.io.monadThrow.monadThrow
import io.kotlintest.specs.StringSpec
import propCheck.*

class TestableSpec : StringSpec({
    "Boolean should be lifted correctly" {
        propCheck {
            forAll { b: Boolean ->
                if (b) Boolean.testable().run { b.property() }
                else expectFailure(b)
            }
        }
    }
})

class PropCheckSpec : StringSpec({
    "propCheckIO should execute the test correctly without throwing exceptions" {
        propCheck {
            forAll { b: Boolean ->
                ioProperty(
                    propCheckIO(Args(maxSuccess = 1)) {
                        Boolean.testable().run { b.property() }
                    }.attempt().map {
                        it.fold({ false }, {
                            when (it) {
                                is Result.Success -> b
                                is Result.Failure -> b.not()
                                is Result.GivenUp -> false
                                is Result.NoExpectedFailure -> false
                            }
                        })
                    }
                )
            }
        }
    }
    "propCheckIOWithError should execute the test correctly and throw an exception" {
        propCheck {
            forAll { b: Boolean ->
                ioProperty(
                    propCheckIOWithError(Args(maxSuccess = 1)) {
                        Boolean.testable().run { b.property() }
                    }.attempt().map {
                        it.fold({ b.not() }, { b })
                    }
                )
            }
        }
    }
    "propCheck is the same as propCheckIOWithError.unsageRunSync()" {
        propCheck {
            forAll { b: Boolean ->
                ioProperty(
                    IO.monadThrow().bindingCatch {
                        propCheck(Args(maxSuccess = 1)) { Boolean.testable().run { b.property() } }
                    }.attempt().map {
                        it.fold({
                            propCheckIOWithError(Args(maxSuccess = 1)) { Boolean.testable().run { b.property() } }.attempt()
                                .unsafeRunSync().fold({ err -> err.message == it.message }, { false })
                        }, {
                            propCheckIOWithError(Args(maxSuccess = 1)) { Boolean.testable().run { b.property() } }.attempt()
                                .unsafeRunSync().fold({ false }, { true })
                        })
                    }
                )
            }
        }
    }
    "propCheckWithResult is the same as propCheckIO.unsageRunSync()" {
        propCheck {
            forAll { b: Boolean ->
                propCheckWithResult(Args(maxSuccess = 1)) { Boolean.testable().run { b.property() } } ==
                        propCheckIO(Args(maxSuccess = 1)) { Boolean.testable().run { b.property() } }.unsafeRunSync()
            }
        }
    }
    "propCheckIO should give up on too many discards" {
        propCheck {
            ioProperty(
                propCheckIO {
                    discardIf(true, false)
                }.attempt().map {
                    it.fold({ false }, { res ->
                        res is Result.GivenUp
                    })
                }
            )
        }
    }
    "propCheckIOWIthError should give up on too many discards and throw" {
        propCheck {
            ioProperty(
                propCheckIOWithError {
                    discardIf(true, false)
                }.attempt().map {
                    it.fold({ true }, { false })
                }
            )
        }
    }
    "propCheckWIthIO should produce the same result with the same random seed" {
        propCheck {
            forAll { tup: Tuple2<Long, Int> ->
                ioProperty(
                    propCheckIO(Args(replay = tup.some())) {
                        forAll { b: Boolean -> b }
                    }.flatMap { res ->
                        propCheckIO(Args(replay = tup.some())) {
                            forAll { b: Boolean -> b }
                        }.map { res == it }
                    }
                )
            }
        }
    }
    "propCheckIO should respect the maxShrinks argument" {
        propCheck {
            forAll { (i): Positive<Byte> ->
                ioProperty(
                    propCheckIO(Args(maxShrinks = i.toInt())) {
                        forAllShrink(arbitraryBoundedLong(), { shrinkLong(it) }) { l: Long ->
                            Math.abs(l) < 20000
                        }
                    }.map {
                        (it is Result.Failure && it.numShrinks + it.numShrinkTries <= i)
                    }
                )
            }
        }
    }
})

