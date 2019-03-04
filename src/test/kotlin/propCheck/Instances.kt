package propCheck

import arrow.core.Option
import arrow.core.Tuple13
import arrow.core.none
import propCheck.assertions.Callback
import propCheck.assertions.Confidence
import propCheck.assertions.TestResult
import propCheck.assertions.TestResult.Companion.fromTuple
import propCheck.assertions.TestResult.Companion.toTuple
import propCheck.instances.arbitrary
import propCheck.instances.fromTup
import propCheck.instances.option.arbitrary.arbitrary
import propCheck.instances.tuple13.arbitrary.arbitrary

object ThrowableArbitrary : Arbitrary<Throwable> {
    override fun arbitrary(): Gen<Throwable> = arbitraryASCIIString().map { Throwable(it) }
    override fun shrink(fail: Throwable): Sequence<Throwable> = shrinkMap({
        it.message!!
    }, {
        Throwable(it)
    }, String.arbitrary()).invoke(fail)
}

interface NoneArbitrary<A> : Arbitrary<Option<A>> {
    override fun arbitrary(): Gen<Option<A>> = Gen.elements(none())
}

val TestResultArbitrary: Arbitrary<TestResult> = fromTup(
    ::toTuple,
    ::fromTuple,
    Tuple13.arbitrary(
        defArbitrary(), defArbitrary(), defArbitrary(), Option.arbitrary(ThrowableArbitrary),
        defArbitrary(), defArbitrary(), object: NoneArbitrary<Confidence> {}, defArbitrary(),
        defArbitrary(), defArbitrary(), defArbitrary(), defArbitrary(),
        object: Arbitrary<List<Callback>> {
            override fun arbitrary(): Gen<List<Callback>> = Gen.sublistOf(emptyList())
        }
    )
)