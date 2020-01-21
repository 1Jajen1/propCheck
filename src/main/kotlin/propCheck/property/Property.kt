package propCheck.property

import arrow.Kind
import arrow.Kind2
import arrow.core.Either
import arrow.core.Id
import arrow.core.extensions.id.monad.monad
import arrow.core.extensions.listk.monoid.monoid
import arrow.extension
import arrow.fx.ForIO
import arrow.mtl.EitherT
import arrow.mtl.WriterT
import arrow.mtl.WriterTPartialOf
import arrow.mtl.extensions.writert.functor.functor
import arrow.mtl.typeclasses.MonadTrans
import arrow.mtl.value
import arrow.typeclasses.*
import pretty.Doc
import pretty.doc
import propCheck.arbitrary.*
import propCheck.arbitrary.`fun`.show.show
import propCheck.arbitrary.gent.alternative.orElse
import propCheck.arbitrary.gent.functor.functor
import propCheck.arbitrary.gent.monad.monad
import propCheck.arbitrary.gent.monadTrans.monadTrans
import propCheck.pretty.showPretty
import propCheck.property.log.monoid.monoid
import propCheck.property.propertyt.monadTest.monadTest
import propCheck.property.testt.monad.monad
import propCheck.property.testt.monadTrans.monadTrans

// -------------- Property

data class Property(val config: PropertyConfig, val prop: PropertyT<ForIO, Unit>) {

    fun mapConfig(f: (PropertyConfig) -> PropertyConfig): Property =
        copy(config = f(config))

    fun withConfidence(c: Confidence): Property =
        mapConfig {
            it.copy(
                terminationCriteria = when (it.terminationCriteria) {
                    is TerminationCriteria.EarlyTermination -> TerminationCriteria.EarlyTermination(c, it.terminationCriteria.limit)
                    is TerminationCriteria.NoEarlyTermination -> TerminationCriteria.NoEarlyTermination(c, it.terminationCriteria.limit)
                    is TerminationCriteria.NoConfidenceTermination -> TerminationCriteria.NoConfidenceTermination(it.terminationCriteria.limit)
                }
            )
        }

    fun withTestLimit(i: TestLimit): Property =
        mapConfig {
            it.copy(
                terminationCriteria = when (it.terminationCriteria) {
                    is TerminationCriteria.EarlyTermination -> TerminationCriteria.EarlyTermination(it.terminationCriteria.confidence, i)
                    is TerminationCriteria.NoEarlyTermination -> TerminationCriteria.NoEarlyTermination(it.terminationCriteria.confidence, i)
                    is TerminationCriteria.NoConfidenceTermination -> TerminationCriteria.NoConfidenceTermination(i)
                }
            )
        }

    fun withDiscardLimit(i: DiscardRatio): Property =
        mapConfig { PropertyConfig.maxDiscardRatio.set(it, i) }

    fun withShrinkLimit(i: ShrinkLimit): Property =
        mapConfig { PropertyConfig.shrinkLimit.set(it, i) }

    fun withShrinkRetries(i: ShrinkRetries): Property =
        mapConfig { PropertyConfig.shrinkRetries.set(it, i) }

    companion object
}

// @higherkind boilerplate
class ForPropertyT private constructor() {
    companion object
}
typealias PropertyTOf<M, A> = arrow.Kind<PropertyTPartialOf<M>, A>
typealias PropertyTPartialOf<M> = arrow.Kind<ForPropertyT, M>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <M, A> PropertyTOf<M, A>.fix(): PropertyT<M, A> =
    this as PropertyT<M, A>

data class PropertyT<M, A>(val unPropertyT: TestT<GenTPartialOf<M>, A>) : PropertyTOf<M, A> {

    fun <B> map(MM: Monad<M>, f: (A) -> B): PropertyT<M, B> = PropertyT(unPropertyT.map(GenT.monad(MM), f))

    fun <B> ap(MM: Monad<M>, ff: PropertyT<M, (A) -> B>): PropertyT<M, B> = PropertyT(unPropertyT.ap(Gen.monad(MM), ff.unPropertyT))

    companion object
}

@extension
interface PropertyTFunctor<M> : Functor<PropertyTPartialOf<M>> {
    fun MM(): Monad<M>

    override fun <A, B> Kind<PropertyTPartialOf<M>, A>.map(f: (A) -> B): Kind<PropertyTPartialOf<M>, B> =
        fix().map(MM(), f)
}

@extension
interface PropertyTApplicative<M> : Applicative<PropertyTPartialOf<M>> {
    fun MM(): Monad<M>

    override fun <A> just(a: A): Kind<PropertyTPartialOf<M>, A> = PropertyT(TestT.just(GenT.monad(MM()), a))

    override fun <A, B> Kind<PropertyTPartialOf<M>, A>.ap(ff: Kind<PropertyTPartialOf<M>, (A) -> B>): Kind<PropertyTPartialOf<M>, B> =
        fix().ap(MM(), ff.fix())
}

@extension
interface PropertyTMonad<M> : Monad<PropertyTPartialOf<M>> {
    fun MM(): Monad<M>

    override fun <A> just(a: A): Kind<PropertyTPartialOf<M>, A> =
        PropertyT(TestT.monad(GenT.monad(MM())).just(a).fix())

    override fun <A, B> Kind<PropertyTPartialOf<M>, A>.flatMap(f: (A) -> Kind<PropertyTPartialOf<M>, B>): Kind<PropertyTPartialOf<M>, B> =
        TestT.monad(GenT.monad(MM())).run {
            PropertyT(
                fix().unPropertyT.flatMap { a ->
                    f(a).fix().unPropertyT
                }.fix()
            )
        }

    override fun <A, B> tailRecM(
        a: A, f: (A) -> Kind<PropertyTPartialOf<M>, Either<A, B>>
    ): Kind<PropertyTPartialOf<M>, B> =
        f(a).flatMap { it.fold({ tailRecM(it, f) }, { just(it) }) }
}

@extension
interface PropertyTAlternative<M> : Alternative<PropertyTPartialOf<M>>, PropertyTApplicative<M> {
    override fun MM(): Monad<M>

    override fun <A> empty(): Kind<PropertyTPartialOf<M>, A> = discard(MM())

    override fun <A> Kind<PropertyTPartialOf<M>, A>.orElse(b: Kind<PropertyTPartialOf<M>, A>): Kind<PropertyTPartialOf<M>, A> =
        PropertyT(
            TestT(
                EitherT(
                    WriterT(
                        fix().unPropertyT.runTestT.value().value().orElse(MM(), b.fix().unPropertyT.runTestT.value().value())
                    )
                )
            )
        )
}

@extension
interface PropertyTMonadTest<M> : MonadTest<PropertyTPartialOf<M>>, PropertyTMonad<M> {
    override fun MM(): Monad<M>

    override fun <A> Test<A>.liftTest(): Kind<PropertyTPartialOf<M>, A> =
        PropertyT(hoist(GenT.monad(MM())))
}

@extension
interface PropertyTMonadTrans : MonadTrans<ForPropertyT> {
    override fun <G, A> Kind<G, A>.liftT(MF: Monad<G>): Kind2<ForPropertyT, G, A> =
        GenT.monadTrans().run { liftT(MF) }.let {
            TestT.monadTrans().run { it.liftT(GenT.monad(MF)).fix() }.let(::PropertyT)
        }
}


// ---------

// these will be specialised later in syntax interfaces so now worries here
fun <M, A> forAllWithT(showA: (A) -> Doc<Markup>, gen: GenT<M, A>, MM: Monad<M>): PropertyT<M, A> =
    PropertyT.monadTest(MM).run {
        PropertyT(TestT.monadTrans().run { gen.liftT(GenT.monad(MM)).fix() }).flatTap { a ->
            writeLog(JournalEntry.Input { showA(a) })
        }.fix()
    }

fun <M, A> forAllWith(showA: (A) -> Doc<Markup>, gen: Gen<A>, MM: Monad<M>): PropertyT<M, A> =
    forAllWithT(showA, gen.generalize(MM), MM)

fun <M, A> forAllT(gen: GenT<M, A>, MM: Monad<M>, SA: Show<A> = Show.any()): PropertyT<M, A> =
    forAllWithT({ SA.run { it.showPretty(SA) } }, gen, MM)

fun <M, A> forAll(gen: Gen<A>, MM: Monad<M>, SA: Show<A> = Show.any()): PropertyT<M, A> =
    forAllT(gen.generalize(MM), MM, SA)

fun <M, A> discard(MM: Monad<M>): PropertyT<M, A> =
    PropertyT(
        TestT(
            EitherT.liftF<WriterTPartialOf<GenTPartialOf<M>, Log>, Failure, A>(
                WriterT.functor(GenT.functor(MM)),
                WriterT.liftF(
                    GenT.monadGen(Id.monad()).discard<A>().fix().generalize(MM),
                    Log.monoid(),
                    GenT.monad(MM)
                )
            )
        )
    )

fun <M, A, B> forAllFn(gen: Gen<Fun<A, B>>, MM: Monad<M>, SA: Show<A> = Show.any(), SB: Show<B> = Show.any()): PropertyT<M, (A) -> B> =
    forAll(gen, MM, Fun.show(SA, SB)).map(MM) { it.component1() }
