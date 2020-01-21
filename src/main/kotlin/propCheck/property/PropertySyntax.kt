package propCheck.property

import arrow.Kind
import arrow.core.Either
import arrow.core.ForId
import arrow.core.Id
import arrow.core.extensions.id.monad.monad
import arrow.fx.ForIO
import arrow.fx.IO
import arrow.fx.IOOf
import arrow.fx.extensions.io.monad.monad
import arrow.typeclasses.Monad
import arrow.typeclasses.MonadContinuation
import arrow.typeclasses.MonadSyntax
import arrow.typeclasses.Show
import pretty.Doc
import propCheck.arbitrary.*
import propCheck.arbitrary.`fun`.show.show
import propCheck.property.propertyt.monad.monad
import kotlin.coroutines.startCoroutine

/*
TODO figure out how to do autobind for all methods without the conflicting overload part
 mainly this means keeping another version of monadtest that autobinds... which sucks
 */

fun property(propertyConfig: PropertyConfig = PropertyConfig(), c: suspend PropertyTestSyntax.() -> Unit): Property {
    val continuation = PropertyTestContinuation<Unit>()
    val wrapReturn: suspend PropertyTestContinuation<*>.() -> PropertyT<ForIO, Unit> = { just(c()).fix() }
    wrapReturn.startCoroutine(continuation, continuation)
    return Property(
        config = propertyConfig,
        prop = continuation.returnedMonad().fix()
    )
}

interface PropertyTestSyntax : MonadSyntax<PropertyTPartialOf<ForIO>>, PropertyTest<ForIO> {
    override fun MM(): Monad<ForIO> = IO.monad()

    suspend fun <A> IO<A>.bind(): A = PropertyT.lift(IO.monad(), this).bind()
}

class PropertyTestContinuation<A> : MonadContinuation<PropertyTPartialOf<ForIO>, A>(
    PropertyT.monad(IO.monad())
), PropertyTestSyntax {
    override fun <A> just(a: A): Kind<PropertyTPartialOf<ForIO>, A> =
        PropertyT.monad(MM()).just(a)

    override fun <A, B> tailRecM(
        a: A,
        f: (A) -> Kind<PropertyTPartialOf<ForIO>, Either<A, B>>
    ): Kind<PropertyTPartialOf<ForIO>, B> =
        PropertyT.monad(MM()).tailRecM(a, f)

    override fun <A, B> Kind<PropertyTPartialOf<ForIO>, A>.flatMap(f: (A) -> Kind<PropertyTPartialOf<ForIO>, B>): Kind<PropertyTPartialOf<ForIO>, B> =
        PropertyT.monad(MM()).run { flatMap(f) }
}

fun <M> PropertyT.Companion.propertyTestM(MM: Monad<M>): PropertyTest<M> = object: PropertyTest<M> {
    override fun MM(): Monad<M> = MM
}

interface PropertyTest<M> : PropertyTMonadTest<M> {
    override fun MM(): Monad<M>

    // forall should give implicit access to MonadTest with the right monad
    //  id for all non-t variants and M in all other cases
    fun <A> forAllWithT(showA: (A) -> Doc<Markup>, gen: GenT<M, A>): PropertyT<M, A> =
        forAllWithT(showA, gen, MM())

    fun <A> forAllWith(showA: (A) -> Doc<Markup>, gen: Gen<A>): PropertyT<M, A> =
        forAllWith(showA, gen, MM())

    fun <A> forAllT(gen: GenT<M, A>, SA: Show<A> = Show.any()): PropertyT<M, A> =
        forAllT(gen, MM(), SA)

    fun <A> forAll(gen: Gen<A>, SA: Show<A> = Show.any()): PropertyT<M, A> =
        forAll(gen, MM(), SA)

    fun <A, B> forAllFn(gen: Gen<Fun<A, B>>, SA: Show<A> = Show.any(), SB: Show<B> = Show.any()): PropertyT<M, (A) -> B> =
        forAllFn(gen, MM(), SA, SB)

    fun <A> forAllWithT(showA: (A) -> Doc<Markup>, f: MonadGen<GenTPartialOf<M>, M>.() -> GenTOf<M, A>) =
        forAllWithT(showA, GenT.monadGen(MM()).f().fix())

    fun <A> forAllWith(showA: (A) -> Doc<Markup>, f: MonadGen<GenTPartialOf<ForId>, ForId>.() -> GenTOf<ForId, A>) =
        forAllWith(showA, GenT.monadGen(Id.monad()).f().fix())

    fun <A> forAllT(SA: Show<A> = Show.any(), f: MonadGen<GenTPartialOf<M>, M>.() -> GenTOf<M, A>) =
        forAllT(GenT.monadGen(MM()).f().fix(), SA)

    fun <A> forAll(SA: Show<A> = Show.any(), f: MonadGen<GenTPartialOf<ForId>, ForId>.() -> GenTOf<ForId, A>) =
        forAll(GenT.monadGen(Id.monad()).f().fix(), SA)

    fun <A, B> forAllFn(SA: Show<A> = Show.any(), SB: Show<B> = Show.any(), f: MonadGen<GenTPartialOf<ForId>, ForId>.() -> GenTOf<ForId, Fun<A, B>>) =
        forAllFn(GenT.monadGen().f().fix(), SA, SB)

    fun <A> discard(): PropertyT<M, A> = discard(MM())

    // TODO lift io and effect { }
    //  implement Monadtrans!
}
