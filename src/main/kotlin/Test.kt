import propCheck.NonPositive
import propCheck.assertions.classify
import propCheck.assertions.forAllShrink
import propCheck.propCheck

fun main() {
    propCheck {
        forAllShrink { (i): NonPositive<Int> ->
            classify(
                i.rem(2) == 0, "evens",
                classify(
                    i.rem(2) != 0, "odds",
                    i > -100
                )
            )
        }
    }
}