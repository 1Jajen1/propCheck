package propCheck.arbitrary

import arrow.Kind
import arrow.core.*
import arrow.extension
import arrow.syntax.collections.tail
import arrow.typeclasses.Contravariant
import arrow.typeclasses.Decidable
import arrow.typeclasses.Divide
import arrow.typeclasses.Divisible

// @higherkind boilerplate
class ForCoarbitrary private constructor() {
    companion object
}
typealias CoarbitraryOf<A> = arrow.Kind<ForCoarbitrary, A>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <A> CoarbitraryOf<A>.fix(): Coarbitrary<A> =
    this as Coarbitrary<A>

interface Coarbitrary<A> : CoarbitraryOf<A> {
    fun <B> Gen<B>.coarbitrary(a: A): Gen<B>

    companion object
}

fun <M, B> GenT<M, B>.variant(i: Long): GenT<M, B> =
    GenT { (rand, size) ->
        runGen(rand.variant(i) toT size)
    }

fun <M> unitCoarbitrary(): Coarbitrary<Unit> = object : Coarbitrary<Unit> {
    override fun <B> Gen<B>.coarbitrary(a: Unit): Gen<B> = this
}

@extension
interface CoarbitraryContravariant : Contravariant<ForCoarbitrary> {
    override fun <A, B> Kind<ForCoarbitrary, A>.contramap(f: (B) -> A): Kind<ForCoarbitrary, B> =
        fix().run {
            object : Coarbitrary<B> {
                override fun <C> Gen<C>.coarbitrary(a: B): Gen<C> = coarbitrary(f(a))
            }
        }
}

@extension
interface CoarbitraryDivide : Divide<ForCoarbitrary>, CoarbitraryContravariant {
    override fun <A, B, Z> divide(
        fa: Kind<ForCoarbitrary, A>,
        fb: Kind<ForCoarbitrary, B>,
        f: (Z) -> Tuple2<A, B>
    ): Kind<ForCoarbitrary, Z> =
        object : Coarbitrary<Z> {
            override fun <C> Gen<C>.coarbitrary(a: Z): Gen<C> = f(a).let { (a, b) ->
                fa.fix().run {
                    fb.fix().run {
                        coarbitrary(b).coarbitrary(a)
                    }
                }
            }
        }
}

@extension
interface CoarbitraryDivisible : Divisible<ForCoarbitrary>, CoarbitraryDivide {
    override fun <A> conquer(): Kind<ForCoarbitrary, A> = object: Coarbitrary<A> {
        override fun <B> Gen<B>.coarbitrary(a: A): Gen<B> = this
    }
}

@extension
interface CoarbitraryDecidable : Decidable<ForCoarbitrary>, CoarbitraryDivisible {
    override fun <A, B, Z> choose(
        fa: Kind<ForCoarbitrary, A>,
        fb: Kind<ForCoarbitrary, B>,
        f: (Z) -> Either<A, B>
    ): Kind<ForCoarbitrary, Z> = object: Coarbitrary<Z> {
        override fun <B> Gen<B>.coarbitrary(a: Z): Gen<B> =
            f(a).fold({ a -> fa.fix().run { coarbitrary(a) } }, { b -> fb.fix().run { coarbitrary(b) } })
    }
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
