package propCheck.arbitrary

import arrow.core.Tuple2
import arrow.core.toT
import arrow.extension

interface Coarbitrary<A> {
    fun <B> Gen<B>.coarbitrary(a: A): Gen<B>
}

fun <B> Gen<B>.variant(i: Long): Gen<B> =
    Gen { (rand, size) ->
        unGen(rand.variant(i) toT size)
    }

fun unitCoarbitrary(): Coarbitrary<Unit> = object: Coarbitrary<Unit> {
    override fun <B> Gen<B>.coarbitrary(a: Unit): Gen<B> = this
}

// keep it here for now, maybe add that for TupleN later
@extension
interface Tuple2Coarbitrary<A, B> : Coarbitrary<Tuple2<A, B>> {
    fun CA(): Coarbitrary<A>
    fun CB(): Coarbitrary<B>
    override fun <C> Gen<C>.coarbitrary(a: Tuple2<A, B>): Gen<C> =
        CA().run {
            CB().run {
                coarbitrary(a.a).coarbitrary(a.b)
            }
        }
}