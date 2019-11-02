package propCheck

import arrow.core.Eval
import arrow.core.some
import arrow.fx.ForIO
import arrow.fx.IO
import arrow.fx.Ref
import arrow.fx.extensions.io.monadDefer.monadDefer
import arrow.fx.fix
import propCheck.arbitrary.Gen

// -----------
sealed class ACT {
    object Inc : ACT()
    object Dec : ACT()
    object Get : ACT()

    override fun toString(): String = when (this) {
        is Inc -> "Inc"
        is Dec -> "Dec"
        is Get -> "Get"
    }
}

data class Counter(var v: Int = 0) {
    fun inc() = ++v
    fun dec() = --v
    fun get() = v
}

class StateSpec : PropertySpec({
    "State-machine-testing should work correctly for a simple counter model" {
        val sm = StateMachine(
            initialState = 0,
            executeAction = { a: ACT, s: Counter ->
                IO {
                    when (a) {
                        is ACT.Inc -> s.inc()
                        is ACT.Dec -> s.dec()
                        is ACT.Get -> s.get()
                    }
                }
            },
            sut = { IO { Counter() } },
            cmdGen = {
                Gen.elements(
                    ACT.Inc,
                    ACT.Dec,
                    ACT.Get
                ).map { it.some() }
            },
            preCondition = { _, _ -> true },
            invariant = { true },
            transition = { s, a ->
                when (a) {
                    is ACT.Get -> s
                    is ACT.Dec -> s - 1
                    is ACT.Inc -> s + 1
                }
            }
        )

        execSeq(sm) { s, a, r ->
            when (a) {
                is ACT.Get -> s == r
                is ACT.Inc -> s + 1 == r
                is ACT.Dec -> s - 1 == r
            }
        }
    }

    "State-machine-testing should fail in parallel for a simple counter model" {
        val sm = StateMachine(
            initialState = 0,
            executeAction = { a: ACT, s: Counter ->
                IO {
                    when (a) {
                        is ACT.Inc -> s.inc()
                        is ACT.Dec -> s.dec()
                        is ACT.Get -> s.get()
                    }
                }
            },
            sut = { IO { Counter() } },
            cmdGen = {
                Gen.elements(
                    ACT.Inc,
                    ACT.Dec,
                    ACT.Get
                ).map { it.some() }
            },
            preCondition = { _, _ -> true },
            invariant = { true },
            transition = { s, a ->
                when (a) {
                    is ACT.Get -> s
                    is ACT.Dec -> s - 1
                    is ACT.Inc -> s + 1
                }
            }
        )

        propCheck.property.expectFailure(
            execPar(sm, 2) { s, a, r ->
                when (a) {
                    is ACT.Get -> s == r
                    is ACT.Inc -> s + 1 == r
                    is ACT.Dec -> s - 1 == r
                }
            }
        )
    }

    "State-machine-testing should work correctly for a arrow-ref" {
        val sm = StateMachine(
            initialState = 0,
            executeAction = { a: ACT, s: Ref<ForIO, Int> ->
                when (a) {
                    is ACT.Inc -> s.updateAndGet { it + 1 }.fix()
                    is ACT.Dec -> s.updateAndGet { it - 1 }.fix()
                    is ACT.Get -> s.get().fix()
                }
            },
            sut = { Ref(IO.monadDefer(), 0).fix() },
            cmdGen = {
                Gen.elements(
                    ACT.Inc,
                    ACT.Dec,
                    ACT.Get
                ).map { it.some() }
            },
            preCondition = { _, _ -> true },
            invariant = { true },
            transition = { s, a ->
                when (a) {
                    is ACT.Get -> s
                    is ACT.Dec -> s - 1
                    is ACT.Inc -> s + 1
                }
            }
        )

        propCheck.property.and(
            execSeq(sm) { s, a, r ->
                when (a) {
                    is ACT.Get -> s == r
                    is ACT.Inc -> s + 1 == r
                    is ACT.Dec -> s - 1 == r
                }
            },
            Eval.later {
                execPar(sm, 2) { s, a, r ->
                    when (a) {
                        is ACT.Get -> s == r
                        is ACT.Inc -> s + 1 == r
                        is ACT.Dec -> s - 1 == r
                    }
                }
            }
        )
    }
})