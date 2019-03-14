package propCheck

import arrow.core.Eval
import arrow.core.Tuple2
import arrow.core.some
import arrow.core.toT
import arrow.effects.IO
import arrow.effects.extensions.io.applicativeError.attempt
import arrow.effects.extensions.io.monadThrow.monadThrow
import propCheck.arbitrary.*

class TestableSpec : PropertySpec({
    "Boolean should be lifted correctly" {
        forAll { b: Boolean ->
            if (b) b.property()
            else expectFailure(b)
        }
    }
})

class PropCheckSpec : PropertySpec({
    "propCheckIO should execute the test correctly without throwing exceptions" {
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
    "propCheckIOWithError should execute the test correctly and throw an exception" {
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
    "propCheck is the same as propCheckIOWithError.unsageRunSync()" {
        forAll { b: Boolean ->
            ioProperty(
                IO.monadThrow().bindingCatch {
                    propCheck(Args(maxSuccess = 1)) { Boolean.testable().run { b.property() } }
                }.attempt().map {
                    it.fold({
                        propCheckIOWithError(Args(maxSuccess = 1)) {
                            Boolean.testable().run { b.property() }
                        }.attempt()
                            .unsafeRunSync().fold(
                                { err ->
                                    counterexample(
                                        "${err.message} =!= ${it.message}",
                                        (err.message == it.message).property()
                                    )
                                },
                                {
                                    counterexample(
                                        "propCheck threw, but propCheckWithIOError did not",
                                        false
                                    )
                                })
                    }, {
                        propCheckIOWithError(Args(maxSuccess = 1)) {
                            Boolean.testable().run { b.property() }
                        }.attempt()
                            .unsafeRunSync().fold({
                                counterexample(
                                    "propCheck did not throw, but propCheckWithIOError did",
                                    false.property()
                                )
                            }, { Boolean.testable().run { true.property() } })
                    })
                }
            )
        }
    }
    "propCheckWithResult is the same as propCheckIO.unsageRunSync()" {
        forAll { b: Boolean ->
            val a = propCheckWithResult(Args(maxSuccess = 1)) { Boolean.testable().run { b.property() } }
            val c = propCheckIO(Args(maxSuccess = 1)) {
                Boolean.testable().run { b.property() }
            }.unsafeRunSync()
            counterexample(
                "$a =!= $c", when (a) {
                    is Result.Success -> c is Result.Success
                    is Result.Failure -> c is Result.Failure
                    else -> false // should not happen at all so fail if it does
                }
            )
        }
    }
    "propCheckIO should give up on too many discards" {
        ioProperty(
            propCheckIO {
                discardIf(true, Eval.now(false.property()))
            }.attempt().map {
                it.fold({ false }, { res ->
                    res is Result.GivenUp
                })
            }
        )
    }
    "propCheckIOWIthError should give up on too many discards and throw" {
        ioProperty(
            propCheckIOWithError {
                discardIf(true, Eval.now(false.property()))
            }.attempt().map {
                it.fold({ true }, { false })
            }
        )
    }
    "propCheckWIthIO should produce the same result with the same random seed" {
        forAll { (tup): Fixed<Tuple2<Long, Int>> ->
            val (l, s) = tup
            val seed = RandSeed(l)
            ioProperty(
                propCheckIO(Args(replay = (seed toT s).some())) {
                    forAll { b: Boolean -> b }
                }.flatMap { res ->
                    propCheckIO(Args(replay = (seed toT s).some())) {
                        forAll { b: Boolean -> b }
                    }.map { it.eqv(res) }
                }
            )
        }
    }
    "propCheckIO should respect the maxShrinks argument" {
        forAll { (i): Positive<Byte> ->
            ioProperty(
                propCheckIO(Args(maxShrinks = i.toInt())) {
                    forAllShrink(arbitraryBoundedLong(), { shrinkLong(it) }, Boolean.testable()) { l: Long ->
                        Math.abs(l) < 20000
                    }
                }.map {
                    (it is Result.Failure && it.numShrinks + it.numShrinkTries <= i).property()
                }
            )
        }
    }
})

