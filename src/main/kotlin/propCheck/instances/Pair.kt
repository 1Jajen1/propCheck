package propCheck.instances

import arrow.core.Tuple2
import arrow.core.toT
import arrow.typeclasses.Show
import propCheck.Arbitrary
import propCheck.Gen
import propCheck.instances.tuple2.arbitrary.arbitrary
import propCheck.shrinkMap

interface PairArbitrary<A, B> : Arbitrary<Pair<A, B>> {
    fun AA(): Arbitrary<A>
    fun AB(): Arbitrary<B>
    override fun arbitrary(): Gen<Pair<A, B>> = Tuple2.arbitrary(AA(), AB()).arbitrary()
        .map { (a, b) -> a to b }

    override fun shrink(fail: Pair<A, B>): Sequence<Pair<A, B>> = shrinkMap({ (a, b) ->
        a toT b
    }, { (a, b) ->
        a to b
    }, Tuple2.arbitrary(AA(), AB())).invoke(fail)
}

interface PairShow<A, B> : Show<Pair<A, B>> {
    fun SA(): Show<A>
    fun SB(): Show<B>
    override fun Pair<A, B>.show(): String =
        "(" + SA().run { first.show() } + "," + SB().run { second.show() } + ")"
}