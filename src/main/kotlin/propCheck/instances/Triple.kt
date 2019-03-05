package propCheck.instances

import arrow.core.Tuple3
import arrow.typeclasses.Show
import propCheck.Arbitrary
import propCheck.Gen
import propCheck.instances.tuple3.arbitrary.arbitrary
import propCheck.shrinkMap

interface TripleArbitrary<A, B, C> : Arbitrary<Triple<A, B, C>> {
    fun AA(): Arbitrary<A>
    fun AB(): Arbitrary<B>
    fun AC(): Arbitrary<C>
    override fun arbitrary(): Gen<Triple<A, B, C>> = Tuple3.arbitrary(AA(), AB(), AC()).arbitrary()
        .map { (a, b, c) -> Triple(a, b, c) }

    override fun shrink(fail: Triple<A, B, C>): Sequence<Triple<A, B, C>> = shrinkMap({ (a, b, c) ->
        Tuple3(a, b, c)
    }, { (a, b, c) ->
        Triple(a, b, c)
    }, Tuple3.arbitrary(AA(), AB(), AC())).invoke(fail)
}

interface TripleShow<A, B, C> : Show<Triple<A, B, C>> {
    fun SA(): Show<A>
    fun SB(): Show<B>
    fun SC(): Show<C>
    override fun Triple<A, B, C>.show(): String =
        "(" + SA().run { first.show() } + "," + SB().run { second.show() } + "," + SC().run { third.show() } + ")"
}