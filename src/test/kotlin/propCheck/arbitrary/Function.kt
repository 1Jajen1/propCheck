package propCheck.arbitrary

import arrow.core.Tuple2
import arrow.core.extensions.eq
import arrow.core.toT
import arrow.typeclasses.Eq
import propCheck.util.PropertySpec
import propCheck.property.*

object FunctionTest {
    fun <A> test(G: Gen<A>, EQA: Eq<A>, AF: Func<A>, AC: Coarbitrary<A>): List<Tuple2<String, Property>> = listOf(
        "determinism" toT property {
            val i = forAll(G).bind()
            val f = forAllFn { int(Range.constant(0, -100, 100)).toFunction(AF, AC) }.bind()

            f(i).eqv(f(i)).bind()
        },
        "Diversity" toT property(PropertyConfig(terminationCriteria = EarlyTermination())) {
            val i = forAll(G).bind()
            val j = forAll(G).bind()
            val f = forAllFn { long(Range.constant(0, Long.MIN_VALUE, Long.MAX_VALUE)).toFunction(AF, AC) }.bind()

            // Coverage takes care of the condition so this is fine. Discard will fail stuff like Unit
            if (EQA.run { i.eqv(j) }) discard<Unit>().bind()
            else {
                cover(100.0, "Different input => Different result", f(i) != f(j)).bind()
            }
        }
    )
}

class BooleanFunctionSpec : PropertySpec({
    "BooleanFunc"(FunctionTest.test(
        Gen.monadGen { boolean() },
        Boolean.eq(),
        Boolean.func(),
        Boolean.coarbitrary())
    )
})

class LongFunctionSpec : PropertySpec({
    "LongFunc"(FunctionTest.test(
        Gen.monadGen { long(Range.constant(0, Long.MIN_VALUE, Long.MAX_VALUE)) },
        Long.eq(),
        Long.func(),
        Long.coarbitrary())
    )
})

// slow...
/*
class ListFunctionSpec : PropertySpec({
    "ListFunc"(FunctionTest.test(
        Gen.monadGen { int(0..100).list(0..100).map { it.k() } },
        ListK.eq(Int.eq()),
        ListK.func(Int.func()),
        ListK.coarbitrary(Int.coarbitrary()))
    )
})
 */
