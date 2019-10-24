package propCheck.instances

import arrow.core.*
import arrow.extension
import arrow.typeclasses.Show
import propCheck.arbitrary.*
import propCheck.arbitrary.tuple2.coarbitrary.coarbitrary
import propCheck.arbitrary.tuple2.func.func
import propCheck.instances.listk.coarbitrary.coarbitrary
import propCheck.instances.listk.func.func

@extension
interface NonEmptyListArbitrary<A> : Arbitrary<NonEmptyList<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<Nel<A>> = AA().arbitrary().listOf().suchThatMap { Nel.fromList(it) }
    override fun shrink(fail: Nel<A>): Sequence<Nel<A>> = shrinkList(fail.all) { AA().shrink(it) }
        .filter { it.isNotEmpty() }.map { Nel.fromListUnsafe(it) }
}

interface NonEmptyListShow<A> : Show<NonEmptyList<A>> {
    fun SA(): Show<A>
    override fun NonEmptyList<A>.show(): String =
            "NonEmptyList(" + all.joinToString { SA().run { it.show() } } + ")"
}

@extension
interface NonEmptyListFunc<A> : Func<NonEmptyList<A>> {
    fun AF(): Func<A>

    override fun <B> function(f: (NonEmptyList<A>) -> B): Fn<NonEmptyList<A>, B> =
        funMap(Tuple2.func(AF(), ListK.func(AF())), {
            Tuple2(it.head, it.tail.k())
        }, { (h, t) ->
            NonEmptyList(h, t)
        }, f)
}

@extension
interface NonEmptyListCoarbitrary<A> : Coarbitrary<NonEmptyList<A>> {
    fun CA(): Coarbitrary<A>
    override fun <B> Gen<B>.coarbitrary(a: Nel<A>): Gen<B> = Tuple2.coarbitrary(CA(), ListK.coarbitrary(CA())).run {
        coarbitrary(a.head toT a.tail.k())
    }
}
