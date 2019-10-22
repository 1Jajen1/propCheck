package propCheck.instances

import arrow.extension
import arrow.typeclasses.Const
import arrow.typeclasses.const
import propCheck.arbitrary.*

@extension
interface ConstArbitrary<A, T> : Arbitrary<Const<A, T>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<Const<A, T>> = AA().arbitrary().map { it.const() }
    override fun shrink(fail: Const<A, T>): Sequence<Const<A, T>> = AA().shrink(fail.value())
        .map { it.const() }
}

@extension
interface ConstFunc<A, T> : Func<Const<A, T>> {
    fun AF(): Func<A>
    override fun <B> function(f: (Const<A, T>) -> B): Fn<Const<A, T>, B> = funMap(AF(), { it.value() }, ::Const, f)
}
