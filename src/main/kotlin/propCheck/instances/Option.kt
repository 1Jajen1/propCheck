package propCheck.instances

import arrow.core.*
import arrow.extension
import arrow.typeclasses.Show
import propCheck.arbitrary.*
import propCheck.instances.either.func.func

@extension
interface OptionArbitrary<A> : Arbitrary<Option<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<Option<A>> = Gen.frequency(
        3 toT AA().arbitrary().map { it.some() },
        1 toT Gen.elements(none())
    )

    override fun shrink(fail: Option<A>): Sequence<Option<A>> = fail.fold({
        emptySequence()
    }, { a ->
        sequenceOf<Option<A>>(none()) + AA().shrink(a).map { it.some() }
    })
}

interface OptionShow<A> : Show<Option<A>> {
    fun SA(): Show<A>
    override fun Option<A>.show(): String = fold({
        "None"
    }, {
        "Some(" + SA().run { it.show() } + ")"
    })
}

@extension
interface OptionFunc<A> : Func<Option<A>> {
    fun AF(): Func<A>

    override fun <B> function(f: (Option<A>) -> B): Fn<Option<A>, B> =
        funMap(Either.func(unitFunc(), AF()), {
            it.fold({ Unit.left() }, { it.right() })
        }, {
            it.fold({ none() }, ::Some)
        }, f)
}

@extension
interface OptionCoarbitrary<A> : Coarbitrary<Option<A>> {
    fun CA(): Coarbitrary<A>

    override fun <B> Gen<B>.coarbitrary(a: Option<A>): Gen<B> = a.fold({
        variant(0)
    }, { a ->
        CA().run { coarbitrary(a).variant(1) }
    })
}
