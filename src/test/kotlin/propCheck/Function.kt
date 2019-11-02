package propCheck

import arrow.core.Eval
import propCheck.arbitrary.Fun
import propCheck.property.cover
import propCheck.property.discardIf
import propCheck.property.forAll

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
                        propCheck.property.checkCoverage(
                            cover(
                                90.0,
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