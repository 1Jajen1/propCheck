package propCheck.assertions

import arrow.core.some
import arrow.effects.IO
import arrow.effects.Ref
import arrow.effects.extensions.io.monad.map
import arrow.effects.extensions.io.monadDefer.monadDefer
import arrow.effects.fix
import io.kotlintest.specs.StringSpec
import propCheck.Gen
import propCheck.propCheck

sealed class CAction {
    object Inc : CAction()
    object Dec : CAction()
    object Get : CAction()
    object Reset : CAction()

    override fun toString(): String = when (this) {
        is Inc -> "Inc"
        is Dec -> "Dec"
        is Get -> "Get"
        is Reset -> "Reset"
    }
}

data class Counter(private var v: Int = 0) {
    fun inc(): Int = ++v
    fun dec(): Int = --v
    fun set(nV: Int): Int {
        v = nV
        return v
    }

    fun get(): Int = v
}

class PoorlyDesignedStateSpec : StringSpec({
    "Arrow ref should not have race conditions" {
        val initialState = 0
        val actionGen = Gen.elements(
            CAction.Inc,
            CAction.Dec,
            CAction.Get,
            CAction.Reset
        ).map { it.some() }
        val preCondition: PreCondition<Int, CAction> = { s, a -> (a is CAction.Dec).not() || (s > 0) }
        val postCondition: PostCondition<Int, CAction, Int> = { s, a, r: Int ->
            when (a) {
                is CAction.Reset -> s == 0
                is CAction.Dec, is CAction.Inc -> s == r
                is CAction.Get -> s == r
            }
        }
        val invariant: Invariant<Int> = { true }
        val transition: Transition<Int, CAction> = { s, a ->
            when (a) {
                is CAction.Get -> s
                is CAction.Inc -> s + 1
                is CAction.Dec -> s - 1
                is CAction.Reset -> 0
            }
        }

        val sm = StateMachine(
            initialState = initialState,
            preCondition = preCondition,
            postCondition = postCondition,
            cmdGen = { actionGen },
            invariant = invariant,
            transition = transition,
            executeAction = { a, ref ->
                when (a) {
                    is CAction.Get -> ref.get().map { it }.fix()
                    is CAction.Inc -> ref.updateAndGet { it + 1 }.fix()
                    is CAction.Dec -> ref.updateAndGet { it - 1 }.fix()
                    is CAction.Reset -> ref.setAndGet(0).fix()
                }
            },
            sut = { Ref.of(0, IO.monadDefer()).fix() }
        )

        propCheck {
            execSeq(sm)
        }

        propCheck(Args(maxSize = 30)) {
            execPar(sm)
        }
    }

    "A normal int counter will have race conditions" {
        val initialState = 0
        val actionGen = Gen.elements(
            CAction.Inc,
            CAction.Dec,
            CAction.Get,
            CAction.Reset
        ).map { it.some() }
        val preCondition: PreCondition<Int, CAction> = { s, a -> (a is CAction.Dec).not() || (s > 0) }
        val postCondition: PostCondition<Int, CAction, Int> = { s, a, r: Int ->
            when (a) {
                is CAction.Reset -> s == 0
                is CAction.Dec, is CAction.Inc -> s == r
                is CAction.Get -> s == r
            }
        }
        val invariant: Invariant<Int> = { true }
        val transition: Transition<Int, CAction> = { s, a ->
            when (a) {
                is CAction.Get -> s
                is CAction.Inc -> s + 1
                is CAction.Dec -> s - 1
                is CAction.Reset -> 0
            }
        }

        // bad model
        val sm2 = StateMachine(
            initialState = initialState,
            preCondition = preCondition,
            postCondition = postCondition,
            cmdGen = { actionGen },
            invariant = invariant,
            transition = transition,
            executeAction = { a, ref ->
                when (a) {
                    is CAction.Get -> IO { ref.get() }
                    is CAction.Inc -> IO { Thread.sleep(2); ref.inc() }
                    is CAction.Dec -> IO { Thread.sleep(2); ref.dec() }
                    is CAction.Reset -> IO { ref.set(0) }
                }
            },
            sut = { IO { Counter() } }
        )

        propCheck {
            execSeq(sm2)
        }

        propCheck(Args(maxSize = 30)) {
            expectFailure(
                execPar(sm2)
            )
        }
    }
})