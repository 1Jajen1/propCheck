package propCheck.arbitrary

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.id.functor.functor
import arrow.core.extensions.id.monad.monad
import arrow.core.extensions.list.traverse.sequence
import arrow.core.extensions.list.traverse.traverse
import arrow.core.extensions.listk.functorFilter.filterMap
import arrow.extension
import arrow.mtl.OptionT
import arrow.mtl.OptionTPartialOf
import arrow.mtl.extensions.optiont.alternative.alternative
import arrow.mtl.extensions.optiont.applicative.applicative
import arrow.mtl.extensions.optiont.functor.functor
import arrow.mtl.extensions.optiont.monad.monad
import arrow.mtl.fix
import arrow.mtl.typeclasses.unnest
import arrow.mtl.value
import arrow.syntax.collections.tail
import arrow.typeclasses.*
import propCheck.arbitrary.gent.alternative.alternative
import propCheck.arbitrary.gent.applicative.applicative
import propCheck.arbitrary.gent.functor.functor
import propCheck.arbitrary.gent.monad.map
import propCheck.arbitrary.gent.monad.monad
import propCheck.property.*
import propCheck.property.rose.alternative.alternative
import propCheck.property.rose.alternative.orElse
import propCheck.property.rose.applicative.applicative
import propCheck.property.rose.birecursive.birecursive
import propCheck.property.rose.monad.monad
import propCheck.property.rose.monadFilter.filterMap
import kotlin.random.Random

// @higherkind boilerplate
class ForGenT private constructor() {
    companion object
}
typealias GenTOf<M, A> = arrow.Kind<GenTPartialOf<M>, A>
typealias GenTPartialOf<M> = arrow.Kind<ForGenT, M>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <M, A> GenTOf<M, A>.fix(): GenT<M, A> =
    this as GenT<M, A>

typealias Gen<A> = GenT<ForId, A>

/**
 * Datatype that modesl creation of a value based on a seed and a size parameter
 */
class GenT<M, A>(val runGen: (Tuple2<RandSeed, Size>) -> Rose<OptionTPartialOf<M>, A>) : GenTOf<M, A> {

    fun <B> genMap(MF: Functor<M>, f: (A) -> B): GenT<M, B> =
        GenT(runGen andThen { r -> r.map(OptionT.functor(MF), f) })

    /**
     * This breaks applicative monad laws because they now behave different, but that
     *  is essential to good shrinking results. And tbh since we assume sameness by just size and same distribution in
     *  monad laws as well, we could consider this equal as well.
     */
    fun <B> genAp(MA: Monad<M>, ff: GenT<M, (A) -> B>): GenT<M, B> = GenT { (seed, size) ->
        val (l, r) = seed.split()
        Rose.applicative(OptionT.applicative(MA)).map(
            this@GenT.runGen(l toT size),
            ff.runGen(r toT size)
        ) { (a, b) -> b(a) }.fix()
    }

    fun <B> genFlatMap(MM: Monad<M>, f: (A) -> GenT<M, B>): GenT<M, B> = GenT { (s, size) ->
        val (l, r) = s.split()
        this@GenT.runGen(r toT size)
            .flatMap(OptionT.monad(MM)) { f(it).runGen(l toT size) }
    }

    fun <B> mapTree(f: (Rose<OptionTPartialOf<M>, A>) -> Rose<OptionTPartialOf<M>, B>): GenT<M, B> = GenT { (r, s) ->
        f(runGen(r toT s))
    }

    companion object {
        fun <M, A> just(MA: Monad<M>, a: A): GenT<M, A> = GenT {
            Rose.just(OptionT.applicative(MA), a)
        }
    }
}

@extension
interface GenTFunctor<M> : Functor<GenTPartialOf<M>> {
    fun FM(): Functor<M>

    override fun <A, B> Kind<GenTPartialOf<M>, A>.map(f: (A) -> B): Kind<GenTPartialOf<M>, B> =
        fix().genMap(FM(), f)
}

@extension
interface GenTApplicative<M> : Applicative<GenTPartialOf<M>> {
    fun MM(): Monad<M>

    override fun <A, B> Kind<GenTPartialOf<M>, A>.ap(ff: Kind<GenTPartialOf<M>, (A) -> B>): Kind<GenTPartialOf<M>, B> =
        fix().genAp(MM(), ff.fix())

    override fun <A> just(a: A): Kind<GenTPartialOf<M>, A> = GenT.just(MM(), a)
}

/**
 * This violates monad laws because every flatMap splits the rng. In practice that is not a problem
 *  because the distribution of A's a Gen produces stays the same.
 * This can however be problematic when using the unsafe methods [promote], [delay].
 */
@extension
interface GenTMonad<M> : Monad<GenTPartialOf<M>> {
    fun MM(): Monad<M>

    // explicit overwrite so I do not use the monadic version here
    override fun <A, B> Kind<GenTPartialOf<M>, A>.map(f: (A) -> B): Kind<GenTPartialOf<M>, B> =
        GenT.functor(MM()).run { map(f) }

    // explicit overwrite so I do not use the monadic version here
    override fun <A, B> Kind<GenTPartialOf<M>, A>.followedBy(fb: Kind<GenTPartialOf<M>, B>): Kind<GenTPartialOf<M>, B> =
        GenT.applicative(MM()).run { followedBy(fb) }

    // explicit overwrite so I do not use the monadic version here
    override fun <A, B> Kind<GenTPartialOf<M>, A>.apTap(fb: Kind<GenTPartialOf<M>, B>): Kind<GenTPartialOf<M>, A> =
        GenT.applicative(MM()).run { apTap(fb) }

    // explicit overwrite so I do not use the monadic version here
    override fun <A, B> Kind<GenTPartialOf<M>, A>.lazyAp(ff: () -> Kind<GenTPartialOf<M>, (A) -> B>): Kind<GenTPartialOf<M>, B> =
        fix().genAp(MM(), ff().fix())

    // explicit overwrite so I do not use the monadic version here
    override fun <A, B> Kind<GenTPartialOf<M>, A>.ap(ff: Kind<GenTPartialOf<M>, (A) -> B>): Kind<GenTPartialOf<M>, B> =
        fix().genAp(MM(), ff.fix())

    override fun <A, B> Kind<GenTPartialOf<M>, A>.flatMap(f: (A) -> Kind<GenTPartialOf<M>, B>): Kind<GenTPartialOf<M>, B> =
        fix().genFlatMap(MM(), f andThen { it.fix() })

    override fun <A> just(a: A): Kind<GenTPartialOf<M>, A> = GenT.just(MM(), a)

    override fun <A, B> tailRecM(a: A, f: (A) -> Kind<GenTPartialOf<M>, Either<A, B>>): Kind<GenTPartialOf<M>, B> =
        f(a).flatMap {
            it.fold({
                tailRecM(it, f)
            }, {
                just(it)
            })
        }
}

@extension
interface GenTSemigroup<M, A> : Semigroup<GenT<M, A>> {
    fun MM(): Monad<M>
    fun AS(): Semigroup<A>

    override fun GenT<M, A>.combine(b: GenT<M, A>): GenT<M, A> = GenT.applicative(MM()).run {
        map(this@combine, b) { (a, b) -> AS().run { a + b } }.fix()
    }
}

@extension
interface GenTMonoid<M, A> : Monoid<GenT<M, A>>, GenTSemigroup<M, A> {
    override fun MM(): Monad<M>
    override fun AS(): Semigroup<A> = AM()
    fun AM(): Monoid<A>

    override fun empty(): GenT<M, A> = GenT.just(MM(), AM().empty())
}

@extension
interface GenTAlternative<M> : Alternative<GenTPartialOf<M>>, GenTApplicative<M> {
    override fun MM(): Monad<M>
    override fun <A> empty(): Kind<GenTPartialOf<M>, A> = GenT { Rose.alternative(OptionT.alternative(MM())).empty<A>().fix() }
    override fun <A> Kind<GenTPartialOf<M>, A>.orElse(b: Kind<GenTPartialOf<M>, A>): Kind<GenTPartialOf<M>, A> =
        GenT { t -> (fix().runGen(t).orElse(OptionT.alternative(MM()), b.fix().runGen(t))) }
}

fun <M> GenT.Companion.monadGen(MM: Monad<M>): MonadGen<GenTPartialOf<M>, M> = object : MonadGen<GenTPartialOf<M>, M> {
    override fun BM(): Monad<M> = MM
    override fun MM(): Monad<GenTPartialOf<M>> = GenT.monad(MM)
    override fun <A> GenT<M, A>.fromGenT(): Kind<GenTPartialOf<M>, A> = this
    override fun <A> Kind<GenTPartialOf<M>, A>.toGenT(): GenT<M, A> = fix()
    override fun <A, B> Kind<GenTPartialOf<M>, A>.map(f: (A) -> B): Kind<GenTPartialOf<M>, B> =
        fix().genMap(BM(), f)
    override fun <A, B> Kind<GenTPartialOf<M>, A>.ap(ff: Kind<GenTPartialOf<M>, (A) -> B>): Kind<GenTPartialOf<M>, B> =
        fix().genAp(BM(), ff.fix())
    override fun <A, B> Kind<GenTPartialOf<M>, A>.flatMap(f: (A) -> Kind<GenTPartialOf<M>, B>): Kind<GenTPartialOf<M>, B> =
        MM().run { flatMap(f) }
    override fun <A> just(a: A): Kind<GenTPartialOf<M>, A> = MM().just(a)
    override fun <A, B> tailRecM(a: A, f: (A) -> Kind<GenTPartialOf<M>, Either<A, B>>): Kind<GenTPartialOf<M>, B> =
        MM().tailRecM(a, f)
    override fun <A> empty(): Kind<GenTPartialOf<M>, A> = GenT.alternative(BM()).empty()
    override fun <A> Kind<GenTPartialOf<M>, A>.orElse(b: Kind<GenTPartialOf<M>, A>): Kind<GenTPartialOf<M>, A> = GenT.alternative(BM()).run {
        orElse(b.fix())
    }
}

fun <M, A> Gen<A>.generalize(MM: Monad<M>): GenT<M, A> = GenT { (r, s) ->
    runGen(r toT s).hoist(object : FunctionK<OptionTPartialOf<ForId>, OptionTPartialOf<M>> {
        override fun <A> invoke(fa: Kind<OptionTPartialOf<ForId>, A>): Kind<OptionTPartialOf<M>, A> =
            OptionT(
                MM.just(fa.fix().value().value())
            )
    }, OptionT.functor(Id.functor()))
}

fun <M, A> GenT.Companion.lift(ff: Functor<M>, fa: Kind<M, A>): GenT<M, A> = GenT {
    Rose.lift(OptionT.functor(ff), OptionT.liftF(ff, fa))
}

interface MonadGen<M, B> : Monad<M>, MonadFilter<M>, Alternative<M> {
    fun MM(): Monad<M>
    fun BM(): Monad<B>

    fun roseM() = OptionT.monad(BM())

    fun <A> GenT<B, A>.fromGenT(): Kind<M, A>
    fun <A> Kind<M, A>.toGenT(): GenT<B, A>

    // generate values without shrinking
    fun <A> generate(f: (RandSeed, Size) -> A): Kind<M, A> = GenT { (r, s) ->
        Rose.just(roseM(), f(r, s))
    }.fromGenT()

    // ------------ shrinking
    // add shrinking capabilities, retaining existing shrinks
    fun <A> Kind<M, A>.shrink(f: (A) -> Sequence<A>): Kind<M, A> = GenT { (r, s) ->
        toGenT().runGen(r toT s).expand(roseM(), f)
    }.fromGenT()

    // throw away shrinking results
    fun <A> Kind<M, A>.prune(): Kind<M, A> = GenT { (r, s) ->
        toGenT().runGen(r toT s).prune(roseM(), 0)
    }.fromGenT()

    // ----------- Size
    fun <A> sized(f: (Size) -> Kind<M, A>): Kind<M, A> = MM().run {
        generate { _, s -> s }.flatMap(f)
    }

    fun <A> Kind<M, A>.resize(i: Size): Kind<M, A> = scale { i }

    fun <A> Kind<M, A>.scale(f: (Size) -> Size): Kind<M, A> = GenT { (r, s) ->
        val newSize = f(s)
        if (newSize.unSize < 0) throw IllegalArgumentException("GenT.scaled. Negative size")
        else toGenT().runGen(r toT newSize)
    }.fromGenT()

    fun <A> Kind<M, A>.small(): Kind<M, A> = scale(::golden)

    fun golden(s: Size): Size = Size((s.unSize * 0.61803398875).toInt())

    // ------- integral numbers
    fun long(range: Range<Long>): Kind<M, Long> =
        long_(range).shrink { it.shrinkTowards(range.origin) }

    fun long_(range: Range<Long>): Kind<M, Long> =
        generate { randSeed, s ->
            val (min, max) = range.bounds(s)
            randSeed.nextLong(min, max).a
        }

    fun long(range: LongRange): Kind<M, Long> = long(Range.constant(range.first, range.last))

    fun long_(range: LongRange): Kind<M, Long> = long_(Range.constant(range.first, range.last))

    fun int(range: Range<Int>): Kind<M, Int> = MM().run {
        long(range.map { it.toLong() }).map { it.toInt() }
    }

    fun int_(range: Range<Int>): Kind<M, Int> = MM().run {
        long_(range.map { it.toLong() }).map { it.toInt() }
    }

    fun int(range: IntRange): Kind<M, Int> = int(Range.constant(range.first, range.last))

    fun int_(range: IntRange): Kind<M, Int> = int_(Range.constant(range.first, range.last))

    fun short(range: Range<Short>): Kind<M, Short> = MM().run {
        long(range.map { it.toLong() }).map { it.toShort() }
    }

    fun short_(range: Range<Short>): Kind<M, Short> = MM().run {
        long_(range.map { it.toLong() }).map { it.toShort() }
    }

    fun byte(range: Range<Byte>): Kind<M, Byte> = MM().run {
        long(range.map { it.toLong() }).map { it.toByte() }
    }

    fun byte_(range: Range<Byte>): Kind<M, Byte> = MM().run {
        long_(range.map { it.toLong() }).map { it.toByte() }
    }

    // floating point numbers
    fun double(range: Range<Double>): Kind<M, Double> =
        double_(range).shrink { it.shrinkTowards(range.origin) }

    fun double_(range: Range<Double>): Kind<M, Double> =
        generate { randSeed, s ->
            val (min, max) = range.bounds(s)
            randSeed.nextDouble(min, max).a
        }

    fun double(range: ClosedFloatingPointRange<Double>): Kind<M, Double> =
        double(Range.constant(range.start, range.endInclusive))

    fun double_(range: ClosedFloatingPointRange<Double>): Kind<M, Double> =
        double_(Range.constant(range.start, range.endInclusive))

    fun float(range: Range<Float>): Kind<M, Float> = MM().run {
        double(range.map { it.toDouble() }).map { it.toFloat() }
    }

    fun float_(range: Range<Float>): Kind<M, Float> = MM().run {
        double_(range.map { it.toDouble() }).map { it.toFloat() }
    }

    fun float(range: ClosedFloatingPointRange<Float>): Kind<M, Float> =
        float(Range.constant(range.start, range.endInclusive))

    fun float_(range: ClosedFloatingPointRange<Float>): Kind<M, Float> =
        float_(Range.constant(range.start, range.endInclusive))

    // boolean
    fun boolean(): Kind<M, Boolean> =
        boolean_().shrink { if (it) sequenceOf(false) else emptySequence() }

    fun boolean_(): Kind<M, Boolean> = generate { randSeed, _ ->
        randSeed.nextInt(0, 1).a != 0
    }

    // chars TODO Arrow codegen bug when trying to generate @extension versions of this
    fun char(range: Range<Char>): Kind<M, Char> = MM().run {
        long(range.map { it.toLong() }).map { it.toChar() }
    }

    fun char_(range: Range<Char>): Kind<M, Char> = MM().run {
        long_(range.map { it.toLong() }).map { it.toChar() }
    }

    fun char(range: CharRange): Kind<M, Char> = char(Range.constant(range.first, range.last))
    fun char_(range: CharRange): Kind<M, Char> = char_(Range.constant(range.first, range.last))

    fun binit(): Kind<M, Char> = char('0'..'1')

    fun octit(): Kind<M, Char> = char('0'..'7')

    fun digit(): Kind<M, Char> = char('0'..'9')

    fun hexit(): Kind<M, Char> = choice(digit(), char('a'..'f'), char('A'..'F'))

    fun lower(): Kind<M, Char> = char('a'..'z')

    fun upper(): Kind<M, Char> = char('A'..'Z')

    fun alpha(): Kind<M, Char> = choice(lower(), upper())

    fun alphaNum(): Kind<M, Char> = choice(lower(), upper(), digit())

    fun ascii(): Kind<M, Char> = MM().run {
        int(0..127).map { it.toChar() }
    }

    fun latin1(): Kind<M, Char> = MM().run {
        int(0..255).map { it.toChar() }
    }

    fun unicode(): Kind<M, Char> = MM().run {
        val s1 = (55296 toT int(0..55295).map { it.toChar() })
        val s2 = (8190 toT int(57344..65533).map { it.toChar() })
        val s3 = (1048576 toT int(65536..1114111).map { it.toChar() })
        frequency(s1, s2, s3)
    }

    fun unicodeAll(): Kind<M, Char> = char(Char.MIN_VALUE..Char.MAX_VALUE)

    fun Kind<M, Char>.string(range: Range<Int>): Kind<M, String> =
        MM().run { list(range).map { it.joinToString("") } }

    fun Kind<M, Char>.string(range: IntRange): Kind<M, String> =
        string(Range.constant(range.first, range.last))

    // combinators
    fun <A> constant(a: A): Kind<M, A> = MM().just(a)

    fun <A> element(vararg els: A): Kind<M, A> =
        if (els.isEmpty()) throw IllegalArgumentException("Gen.Element used with no arguments")
        else MM().fx.monad {
            val i = !int(Range.constant(0, els.size - 1))
            els[i]
        }

    fun <A> choice(vararg gens: Kind<M, A>): Kind<M, A> =
        if (gens.isEmpty()) throw IllegalArgumentException("Gen.Choice used with no arguments")
        else MM().fx.monad {
            val i = !int(Range.constant(0, gens.size))
            !gens[i]
        }

    fun <A> frequency(vararg gens: Tuple2<Int, Kind<M, A>>): Kind<M, A> =
        if (gens.isEmpty()) throw IllegalArgumentException("Gen.Frequency used with no arguments")
        else MM().fx.monad {
            val total = gens.map { it.a }.sum()
            val n = !int(Range.constant(0, total))
            !gens.toList().pick(n)
        }

    private fun <A> List<Tuple2<Int, A>>.pick(n: Int): A =
        if (isEmpty()) throw IllegalArgumentException("Gen.Frequency.Pick used with no arguments")
        else first().let { (k, el) ->
            if (n <= k) el
            else tail().pick(n - k)
        }

    fun <A> recursive(
        chooseFn: (List<Kind<M, A>>) -> Kind<M, A>,
        nonRec: List<Kind<M, A>>,
        rec: () -> List<Kind<M, A>>
    ): Kind<M, A> =
        sized { s ->
            if (s.unSize <= 1) chooseFn(nonRec)
            else chooseFn(nonRec + rec().map { it.small() })
        }

    fun <A> discard(): Kind<M, A> = GenT { _ ->
        Rose<OptionTPartialOf<B>, A>(OptionT.none(BM()))
    }.fromGenT()

    fun <A> Kind<M, A>.ensure(p: (A) -> Boolean): Kind<M, A> =
        MM().fx.monad {
            val x = !this@ensure
            if (p(x)) x
            else !discard<A>()
        }

    override fun <A, C> Kind<M, A>.filterMap(f: (A) -> Option<C>): Kind<M, C> {
        fun t(k: Int): Kind<M, C> =
            if (k > 100) discard()
            else fx {
                val (x, gen) = this@filterMap.scale { Size(2 * k + it.unSize) }.freeze().bind()
                f(x).fold({ t(k + 1).bind() }, {
                    gen.toGenT()
                        .mapTree { it.filterMap(OptionT.alternative(BM()), OptionT.monad(BM()), f)  }
                        .fromGenT().bind()
                })
            }
        return t(0)
    }

    fun <A> Kind<M, A>.option(): Kind<M, Option<A>> = sized { n ->
        frequency(
            2 toT MM().just(None),
            1 + n.unSize toT MM().run { this@option.map { it.some() } }
        )
    }

    fun <A> Kind<M, A>.list(range: Range<Int>): Kind<M, List<A>> = sized { s ->
        MM().run {
            fx.monad {
                val n = !int_(range)
                !this@list.toGenT().mapTree {
                    Rose.just(OptionT.monad(BM()), it)
                }.fromGenT().replicate(n)
            }.toGenT().mapTree { r ->
                Rose(
                    roseM().run {
                        r.runRose.flatMap {
                            it.res
                                .traverse(OptionT.monad(BM())) { it.runRose }
                                .map {
                                    it.fix().asSequence().interleave(OptionT.monad(BM()))
                                }
                        }
                    }
                )
            }.fromGenT()
                .map { it.toList() }
                .ensure { it.size >= range.lowerBound(s) }
        }
    }

    fun <A> Kind<M, A>.list(range: IntRange): Kind<M, List<A>> = list(Range.constant(range.first, range.last))

    fun <A> Kind<M, A>.set(range: Range<Int>): Kind<M, Set<A>> = MM().run {
        map { it toT Unit }.map(range).map { it.keys }
    }

    fun <K, A> Kind<M, Tuple2<K, A>>.map(range: Range<Int>): Kind<M, Map<K, A>> = sized { s ->
        MM().run {
            fx.monad {
                val k = !int_(range)
                !this@map.uniqueByKey(k)
            }.shrink { it.shrink() }
                .flatMap { it.sequence(MM()).map { it.fix() } }
                .map { it.toMap() }
                .ensure { it.size >= range.lowerBound(s) }
        }
    }

    fun <K, A> Kind<M, Tuple2<K, A>>.uniqueByKey(n: Int): Kind<M, List<Kind<M, Tuple2<K, A>>>> {
        fun go(k: Int, map: Map<K, Kind<M, Tuple2<K, A>>>): Kind<M, List<Kind<M, Tuple2<K, A>>>> =
            if (k > 100) discard()
            else freeze().replicate(n).flatMap {
                val res = (map + it.map { it.bimap({ it.a }, ::identity) }.toMap())
                if (res.size >= n) MM().just(res.values.toList())
                else go(k + 1, res)
            }
        return go(0, emptyMap())
    }

    // subterms
    fun <A> Kind<M, A>.freeze(): Kind<M, Tuple2<A, Kind<M, A>>> =
        GenT { (r, s) ->
            Rose.monad(roseM()).fx.monad {
                val mx =
                    !Rose.lift(roseM(), OptionT.liftF(BM(), toGenT().runGen(r toT s).runRose.fix().value()))
                mx.fold({
                    !Rose.alternative(OptionT.alternative(BM())).empty<Tuple2<A, Kind<M, A>>>()
                }, {
                    (it.res toT GenT { _ ->
                        Rose(OptionT.monad(BM()).just(it))
                    }.fromGenT())
                })
            }.fix()
        }.fromGenT()

    // invariant: List size is never changed!
    fun <A> List<Kind<M, A>>.genSubterms(): Kind<M, Subterms<A>> =
        MM().run {
            traverse(MM()) { it.freeze().map { it.b } }
                .map { Subterms.All(it.fix()) }
                .shrink<Subterms<Kind<M, A>>> { it.shrinkSubterms() }
                .flatMap {
                    when (it) {
                        is Subterms.One -> it.a.map { Subterms.One(it) }
                        is Subterms.All -> it.l
                            .sequence(MM())
                            .map { Subterms.All(it.fix()) }
                    }
                }
        }

    // invariant: size list in f is always equal to the size of this
    fun <A> List<Kind<M, A>>.subtermList(f: (List<A>) -> Kind<M, A>): Kind<M, A> =
        MM().run {
            this@subtermList.genSubterms().flatMap { it.fromSubterms(MM(), f) }
        }

    fun <A> Kind<M, A>.subtermM(f: (A) -> Kind<M, A>): Kind<M, A> =
        listOf(this).subtermList { f(it.first()) }

    fun <A> Kind<M, A>.subterm(f: (A) -> A): Kind<M, A> =
        subtermM { MM().just(f(it)) }

    fun <A> subtermM2(g1: Kind<M, A>, g2: Kind<M, A>, f: (A, A) -> Kind<M, A>): Kind<M, A> =
        MM().run { listOf(g1, g2).genSubterms().flatMap { it.fromSubterms(MM()) { f(it[0], it[1]) } } }

    fun <A> subterm2(g1: Kind<M, A>, g2: Kind<M, A>, f: (A, A) -> A): Kind<M, A> =
        subtermM2(g1, g2) { a1, a2 -> MM().just(f(a1, a2)) }

    fun <A> subtermM3(g1: Kind<M, A>, g2: Kind<M, A>, g3: Kind<M, A>, f: (A, A, A) -> Kind<M, A>): Kind<M, A> =
        MM().run { listOf(g1, g2, g3).genSubterms().flatMap { it.fromSubterms(MM()) { f(it[0], it[1], it[2]) } } }

    fun <A> subterm3(g1: Kind<M, A>, g2: Kind<M, A>, g3: Kind<M, A>, f: (A, A, A) -> A): Kind<M, A> =
        subtermM3(g1, g2, g3) { a1, a2, a3 -> MM().just(f(a1, a2, a3)) }

    // permutations
    fun <A> List<A>.subsequence(): Kind<M, List<A>> =
        MM().run {
            traverse(MM()) { a ->
                boolean_().map { if (it) a.some() else None }
            }.map { it.filterMap(::identity) as List<A> }
                .shrink { it.shrink() }
        }

    // typeclass overwrites for easier use
    fun <A> fx(f: suspend MonadSyntax<M>.() -> A): Kind<M, A> =
        MM().fx.monad(f)
}

sealed class Subterms<A> {
    data class One<A>(val a: A) : Subterms<A>()
    data class All<A>(val l: List<A>) : Subterms<A>()
}

fun <M, A> Subterms<A>.fromSubterms(AM: Applicative<M>, f: (List<A>) -> Kind<M, A>): Kind<M, A> = when (this) {
    is Subterms.One -> AM.just(a)
    is Subterms.All -> f(l)
}

fun <A> Subterms<A>.shrinkSubterms(): Sequence<Subterms<A>> = when (this) {
    is Subterms.One -> emptySequence()
    is Subterms.All -> l.asSequence().map { Subterms.One(it) }
}

// samples // No point for IO imo as these are debug only anyway
// if you need them in IO just wrap them in IO { ... }
fun <A> Gen<A>.sample(): A {
    tailrec fun loop(n: Int): A =
        if (n <= 0) throw IllegalStateException("Gen.Sample too many discards")
        else {
            val seed = RandSeed(Random.nextLong())
            val opt = this.runGen(seed toT Size(30)).runRose
                .value() // optionT
                .value() // id
            when (opt) {
                is None -> loop(n - 1)
                is Some -> opt.t.res
            }
        }
    return loop(100)
}

fun <A> Gen<A>.print(SA: Show<A> = Show.any()): Unit =
    printWith(Size(30), RandSeed(Random.nextLong()), SA)

fun <A> Gen<A>.printWith(size: Size, r: RandSeed, SA: Show<A> = Show.any()): Unit =
    this.runGen(r toT size).runRose
        .value() // optionT
        .value() // id
        .fold({
            println("Result:")
            println("<discard>")
        }, {
            println("Result:")
            println(SA.run { it.res.show() })
            println("Shrinks:")
            it.shrunk
                .forEach {
                    it.runRose
                        .value()
                        .value()
                        .map {
                            println(SA.run { it.res.show() })
                        }
                }
        })

fun <A> Gen<A>.printTree(SA: Show<A> = Show.any()): Unit =
    printTreeWith(Size(30), RandSeed(Random.nextLong()), SA)

fun <A> Gen<A>.printTreeWith(size: Size, randSeed: RandSeed, SA: Show<A> = Show.any()) =
    runGen(randSeed toT size)
        .let {
            Rose.birecursive<OptionTPartialOf<ForId>, A>(OptionT.monad(Id.monad())).run {
                it.cata<List<String>> {
                    it.unnest()
                        .value()
                        .value()
                        .fold({
                            listOf("<discarded>")
                        }, { r ->
                            // TODO wooo ugly code, refractor to a seperate method
                            SA.run {
                                listOf(
                                    r.fix().res.show()
                                ) + r.fix().shrunk.toList()
                                    .let { l ->
                                        l.dropLast(1)
                                            .flatMap { ls ->
                                                when {
                                                    ls.isEmpty() -> emptyList()
                                                    else -> {
                                                        listOf(
                                                            "|-> ${ls.first()}"
                                                        ) + ls.tail()
                                                            .map { "|   $it" }
                                                    }
                                                }
                                            } + (
                                                if (l.isEmpty()) emptyList()
                                                else l.last().let { ls ->
                                                    when {
                                                        ls.isEmpty() -> emptyList()
                                                        else -> {
                                                            listOf(
                                                                "--> ${ls.first()}"
                                                            ) + ls.tail()
                                                                .map { "    $it" }
                                                        }
                                                    }
                                                })
                                    }
                            }
                        })
                }
            }
        }
        .joinToString("\n")
        .let(::println)

// TODO fix range 3 to 3
fun main() {
    treeGen()
        .printTreeWith(Size(3), RandSeed(1))
}

sealed class Tree<A> {
    data class Leaf<A>(val a: A) : Tree<A>()
    data class Branch<A>(val l: Tree<A>, val r: Tree<A>) : Tree<A>()
}

fun treeGen(): Gen<Tree<Int>> = Gen.monadGen(Id.monad()).run {
    recursive(
        { els -> choice(*els.toTypedArray()) },
        listOf(
            int(0..10).map(Id.monad()) { Tree.Leaf(it) as Tree<Int> }.fix()
        ), {
            listOf(
                //Gen.applicative(Id.monad()).map(treeGen(), treeGen()) { (l, r) -> Tree.Branch(l, r) }.fix()
                subterm2(treeGen(), treeGen()) { l, r -> Tree.Branch(l, r) }.fix()
            )
        }
    ).fix()
}
