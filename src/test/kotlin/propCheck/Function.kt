package propCheck

import arrow.core.Eval
import propCheck.arbitrary.Fun

class FunctionSpec : PropertySpec({
    "a function outputs the same for the same input" {
        forAll { (f): Fun<Int, Int> ->
            forAll { i: Int ->
                f(i) == f(i)
            }
        }
    }
    "a generated function outputs different values with different inputs" {
        forAll { (f): Fun<Int, Int> ->
            forAll { (i, j): Pair<Int, Int> ->
                discardIf(
                    i == j,
                    Eval.later {
                        checkCoverage(
                            cover(
                                95.0,
                                f(i) != f(j),
                                "different in => different out",
                                true
                            )
                        )
                    }
                )
            }
        }
    }
})