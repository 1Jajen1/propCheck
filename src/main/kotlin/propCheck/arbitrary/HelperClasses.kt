package propCheck.arbitrary

import arrow.Kind
import arrow.core.Tuple2
import arrow.core.extensions.eq
import arrow.core.extensions.order
import arrow.data.ListK
import arrow.data.extensions.list.eq.eqv
import arrow.data.extensions.listk.eq.eq
import arrow.data.extensions.sequence.foldable.isEmpty
import arrow.data.k
import arrow.extension
import arrow.typeclasses.Eq
import arrow.typeclasses.Functor
import arrow.typeclasses.Order
import arrow.typeclasses.Show
import propCheck.arbitrary.blind.show.show
import propCheck.instances.arbitrary
import propCheck.instances.listk.arbitrary.arbitrary

// ---------------- Blind
// @higherkind boilerplate
class ForBlind private constructor() {
    companion object
}
typealias BlindOf<A> = arrow.Kind<ForBlind, A>
typealias BlindKindedJ<A> = io.kindedj.Hk<ForBlind, A>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <A> BlindOf<A>.fix(): Blind<A> =
    this as Blind<A>

/**
 * A's but without showing its value
 * Useful to reduce clutter
 */
data class Blind<A>(val a: A) : BlindOf<A> {
    override fun toString(): String = Blind.show<A>().run { show() }

    companion object
}

@extension
interface BlindFunctor : Functor<ForBlind> {
    override fun <A, B> Kind<ForBlind, A>.map(f: (A) -> B): Kind<ForBlind, B> =
        Blind(f(fix().a))
}

@extension
interface BlindEq<A> : Eq<Blind<A>> {
    fun EQA(): Eq<A>
    override fun Blind<A>.eqv(b: Blind<A>): Boolean = EQA().run {
        a.eqv(b.a)
    }
}

@extension
interface BlindOrder<A> : Order<Blind<A>> {
    fun OA(): Order<A>
    override fun Blind<A>.compare(b: Blind<A>): Int = OA().run {
        a.compare(b.a)
    }
}

@extension
interface BlindShow<A> : Show<Blind<A>> {
    override fun Blind<A>.show(): String = "(*)"
}

@extension
interface BlindArbitrary<A> : Arbitrary<Blind<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<Blind<A>> = AA().arbitrary().map {
        Blind(
            it
        )
    }
    override fun shrink(fail: Blind<A>): Sequence<Blind<A>> = AA().shrink(fail.a).map {
        Blind(
            it
        )
    }
}

// --------------- Fixed
// @higherkind boilerplate
class ForFixed private constructor() {
    companion object
}
typealias FixedOf<A> = arrow.Kind<ForFixed, A>
typealias FixedKindedJ<A> = io.kindedj.Hk<ForFixed, A>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <A> FixedOf<A>.fix(): Fixed<A> =
    this as Fixed<A>

/**
 * A's but without shrinking
 * Useful to prevent shrinking without introducing a new arbitrary instance
 */
data class Fixed<A>(val a: A) : FixedOf<A> {
    companion object
}

@extension
interface FixedFunctor : Functor<ForFixed> {
    override fun <A, B> Kind<ForFixed, A>.map(f: (A) -> B): Kind<ForFixed, B> =
        Fixed(f(fix().a))
}

@extension
interface FixedEq<A> : Eq<Fixed<A>> {
    fun EQA(): Eq<A>
    override fun Fixed<A>.eqv(b: Fixed<A>): Boolean = EQA().run {
        a.eqv(b.a)
    }
}

@extension
interface FixedOrder<A> : Order<Fixed<A>> {
    fun OA(): Order<A>
    override fun Fixed<A>.compare(b: Fixed<A>): Int = OA().run {
        a.compare(b.a)
    }
}

@extension
interface FixedShow<A> : Show<Fixed<A>> {
    fun SA(): Show<A>
    override fun Fixed<A>.show(): String = SA().run { a.show() }
}

@extension
interface FixedArbitrary<A> : Arbitrary<Fixed<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<Fixed<A>> = AA().arbitrary().map {
        Fixed(
            it
        )
    }
}

// ----------------------- Ordered list
// @higherkind boilerplate
class ForOrderedList private constructor() {
    companion object
}
typealias OrderedListOf<A> = arrow.Kind<ForOrderedList, A>
typealias OrderedListKindedJ<A> = io.kindedj.Hk<ForOrderedList, A>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <A> OrderedListOf<A>.fix(): OrderedList<A> =
    this as OrderedList<A>

/**
 * Generates only sorted lists of A's
 */
data class OrderedList<A>(val l: List<A>) : OrderedListOf<A> {
    companion object
}

@extension
interface OrderedListFunctor : Functor<ForOrderedList> {
    override fun <A, B> Kind<ForOrderedList, A>.map(f: (A) -> B): Kind<ForOrderedList, B> =
        OrderedList(fix().l.map(f))
}

@extension
interface OrderedListEq<A> : Eq<OrderedList<A>> {
    fun EQA(): Eq<A>
    override fun OrderedList<A>.eqv(b: OrderedList<A>): Boolean =
        ListK.eq(EQA()).run { l.k().eqv(b.l.k()) }
}

@extension
interface OrderedListShow<A> : Show<OrderedList<A>> {
    fun SA(): Show<A>
    override fun OrderedList<A>.show(): String = "[ " + l.joinToString { SA().run { it.show() } } + " ]"
}

@extension
interface OrderedListArbitrary<A> : Arbitrary<OrderedList<A>> {
    fun OA(): Order<A>
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<OrderedList<A>> = ListK.arbitrary(AA()).arbitrary().map {
        OrderedList(it.sortedWith(Comparator { p0, p1 -> OA().run { p0.compare(p1) } }))
    }

    override fun shrink(fail: OrderedList<A>): Sequence<OrderedList<A>> =
        ListK.arbitrary(AA()).shrink(fail.l.k()).filter {
            it.eqv(OA(), it.sortedWith(Comparator { p0, p1 -> OA().run { p0.compare(p1) } }))
        }.map { OrderedList(it) }
}

// ---------- Smart
// @higherkind boilerplate
class ForSmart private constructor() {
    companion object
}
typealias SmartOf<A> = arrow.Kind<ForSmart, A>
typealias SmartKindedJ<A> = io.kindedj.Hk<ForSmart, A>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <A> SmartOf<A>.fix(): Smart<A> =
    this as Smart<A>

/**
 * A's but with a different approach to shrinking
 * Shuffles the order a bit
 */
data class Smart<A>(val i: Int, val a: A) : SmartOf<A> {
    companion object
}

@extension
interface SmartFunctor : Functor<ForSmart> {
    override fun <A, B> Kind<ForSmart, A>.map(f: (A) -> B): Kind<ForSmart, B> = fix().let {
        Smart(it.i, f(it.a))
    }
}

@extension
interface SmartShow<A> : Show<Smart<A>> {
    fun SA(): Show<A>
    override fun Smart<A>.show(): String = SA().run { a.show() }
}

@extension
interface SmartArbitrary<A> : Arbitrary<Smart<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<Smart<A>> = AA().arbitrary().map {
        Smart(
            0,
            it
        )
    }
    override fun shrink(fail: Smart<A>): Sequence<Smart<A>> = AA().shrink(fail.a).mapIndexed { index, a ->
        Smart(index, a)
    }.let {
        val i = Math.max(0, fail.i - 2)
        fun <B> Sequence<B>.ilv(other: Sequence<B>): Sequence<B> = when {
            isEmpty() -> other
            other.isEmpty() -> this
            else -> sequenceOf(first()) + sequenceOf(other.first()) + (drop(1).ilv(other.drop(1)))
        }
        it.take(i).ilv(it.drop(i))
    }
}

// ---------- Shrink2
// @higherkind boilerplate
class ForShrink2 private constructor() {
    companion object
}
typealias Shrink2Of<A> = arrow.Kind<ForShrink2, A>
typealias Shrink2KindedJ<A> = io.kindedj.Hk<ForShrink2, A>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <A> Shrink2Of<A>.fix(): Shrink2<A> =
    this as Shrink2<A>

/**
 * A's but performs two shrinking steps at once
 */
data class Shrink2<A>(val a: A) : Shrink2Of<A> {
    companion object
}

@extension
interface Shrink2Functor : Functor<ForShrink2> {
    override fun <A, B> Kind<ForShrink2, A>.map(f: (A) -> B): Kind<ForShrink2, B> =
        Shrink2(f(fix().a))
}

@extension
interface Shrink2Eq<A> : Eq<Shrink2<A>> {
    fun EQA(): Eq<A>
    override fun Shrink2<A>.eqv(b: Shrink2<A>): Boolean = EQA().run {
        a.eqv(b.a)
    }
}

@extension
interface Shrink2Order<A> : Order<Shrink2<A>> {
    fun OA(): Order<A>
    override fun Shrink2<A>.compare(b: Shrink2<A>): Int = OA().run {
        a.compare(b.a)
    }
}

@extension
interface Shrink2Show<A> : Show<Shrink2<A>> {
    fun SA(): Show<A>
    override fun Shrink2<A>.show(): String = SA().run {
        a.show()
    }
}

@extension
interface Shrink2Arbitrary<A> : Arbitrary<Shrink2<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<Shrink2<A>> = AA().arbitrary().map {
        Shrink2(
            it
        )
    }
    override fun shrink(fail: Shrink2<A>): Sequence<Shrink2<A>> = AA().shrink(fail.a).let {
        it + it.flatMap { AA().shrink(it) }
    }.map { Shrink2(it) }
}

// ------------ Shrinking
// @higherkind boilerplate
class ForShrinking private constructor() {
    companion object
}
typealias ShrinkingOf<S, A> = arrow.Kind<ShrinkingPartialOf<S>, A>
typealias ShrinkingPartialOf<S> = arrow.Kind<ForShrinking, S>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <S, A> ShrinkingOf<S, A>.fix(): Shrinking<S, A> =
    this as Shrinking<S, A>

/**
 * A's but shrinking now carries a state
 */
data class Shrinking<S, A>(val s: S, val a: A) : ShrinkingOf<S, A> {
    companion object
}

@extension
interface ShrinkingFunctor<S> : Functor<ShrinkingPartialOf<S>> {
    override fun <A, B> Kind<ShrinkingPartialOf<S>, A>.map(f: (A) -> B): Kind<ShrinkingPartialOf<S>, B> = fix().let {
        Shrinking(it.s, f(it.a))
    }
}

@extension
interface ShrinkingShow<S, A> : Show<Shrinking<S, A>> {
    fun SA(): Show<A>
    override fun Shrinking<S, A>.show(): String = SA().run { a.show() }
}

@extension
interface ShrinkingArbitrary<S, A> : Arbitrary<Shrinking<S, A>> {
    fun AA(): Arbitrary<A>
    fun SS(): ShrinkState<S, A>
    override fun arbitrary(): Gen<Shrinking<S, A>> = AA().arbitrary().map {
        Shrinking(
            SS().shrinkInit(it),
            it
        )
    }
    override fun shrink(fail: Shrinking<S, A>): Sequence<Shrinking<S, A>> = SS().shrinkState(fail.a, fail.s)
        .map { Shrinking(it.b, it.a) }
}

interface ShrinkState<S, A> {
    fun shrinkInit(a: A): S
    fun shrinkState(a: A, state: S): Sequence<Tuple2<A, S>>
}

// --------------- Positive numbers > 0
// @higherkind boilerplate
class ForPositive private constructor() {
    companion object
}
typealias PositiveOf<A> = arrow.Kind<ForPositive, A>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <A> PositiveOf<A>.fix(): Positive<A> =
    this as Positive<A>

/**
 * A's but shrinking now carries a state
 */
data class Positive<A>(val a: A) : PositiveOf<A> {
    companion object
}

@extension
interface PositiveFunctor : Functor<ForPositive> {
    override fun <A, B> Kind<ForPositive, A>.map(f: (A) -> B): Kind<ForPositive, B> =
        Positive(f(fix().a))
}

@extension
interface PositiveEq<A> : Eq<Positive<A>> {
    fun EQA(): Eq<A>
    override fun Positive<A>.eqv(b: Positive<A>): Boolean = EQA().run {
        a.eqv(b.a)
    }
}

@extension
interface PositiveShow<A> : Show<Positive<A>> {
    fun SA(): Show<A>
    override fun Positive<A>.show(): String = SA().run { a.show() }
}

@extension
interface PositiveArbitrary<A : Number> : Arbitrary<Positive<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<Positive<A>> = AA().arbitrary().suchThat { it.toDouble() > 0 }.map {
        Positive(
            it
        )
    }

    override fun shrink(fail: Positive<A>): Sequence<Positive<A>> = AA().shrink(fail.a).filter { it.toDouble() > 0 }
        .map { Positive(it) }
}

// --------------- Non negative numbers >= 0
// @higherkind boilerplate
class ForNonNegative private constructor() {
    companion object
}
typealias NonNegativeOf<A> = arrow.Kind<ForNonNegative, A>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <A> NonNegativeOf<A>.fix(): NonNegative<A> =
    this as NonNegative<A>

/**
 * A's but shrinking now carries a state
 */
data class NonNegative<A>(val a: A) : NonNegativeOf<A> {
    companion object
}

@extension
interface NonNegativeFunctor : Functor<ForNonNegative> {
    override fun <A, B> Kind<ForNonNegative, A>.map(f: (A) -> B): Kind<ForNonNegative, B> =
        NonNegative(f(fix().a))
}

@extension
interface NonNegativeEq<A> : Eq<NonNegative<A>> {
    fun EQA(): Eq<A>
    override fun NonNegative<A>.eqv(b: NonNegative<A>): Boolean = EQA().run {
        a.eqv(b.a)
    }
}

@extension
interface NonNegativeShow<A> : Show<NonNegative<A>> {
    fun SA(): Show<A>
    override fun NonNegative<A>.show(): String = SA().run { a.show() }
}

@extension
interface NonNegativeArbitrary<A : Number> : Arbitrary<NonNegative<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<NonNegative<A>> =
        AA().arbitrary().suchThat { it.toDouble() >= 0 }.map { NonNegative(it) }

    override fun shrink(fail: NonNegative<A>): Sequence<NonNegative<A>> =
        AA().shrink(fail.a).filter { it.toDouble() >= 0 }
            .map { NonNegative(it) }
}

// --------------- Negative numbers < 0
// @higherkind boilerplate
class ForNegative private constructor() {
    companion object
}
typealias NegativeOf<A> = arrow.Kind<ForNegative, A>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <A> NegativeOf<A>.fix(): Negative<A> =
    this as Negative<A>

/**
 * A's but shrinking now carries a state
 */
data class Negative<A>(val a: A) : NegativeOf<A> {
    companion object
}

@extension
interface NegativeFunctor : Functor<ForNegative> {
    override fun <A, B> Kind<ForNegative, A>.map(f: (A) -> B): Kind<ForNegative, B> =
        Negative(f(fix().a))
}

@extension
interface NegativeEq<A> : Eq<Negative<A>> {
    fun EQA(): Eq<A>
    override fun Negative<A>.eqv(b: Negative<A>): Boolean = EQA().run {
        a.eqv(b.a)
    }
}

@extension
interface NegativeShow<A> : Show<Negative<A>> {
    fun SA(): Show<A>
    override fun Negative<A>.show(): String = SA().run { a.show() }
}

@extension
interface NegativeArbitrary<A : Number> : Arbitrary<Negative<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<Negative<A>> = AA().arbitrary().suchThat { it.toDouble() < 0 }.map {
        Negative(
            it
        )
    }

    override fun shrink(fail: Negative<A>): Sequence<Negative<A>> = AA().shrink(fail.a).filter { it.toDouble() < 0 }
        .map { Negative(it) }
}

// --------------- Non positive numbers <= 0
// @higherkind boilerplate
class ForNonPositive private constructor() {
    companion object
}
typealias NonPositiveOf<A> = arrow.Kind<ForNonPositive, A>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <A> NonPositiveOf<A>.fix(): NonPositive<A> =
    this as NonPositive<A>

/**
 * A's but shrinking now carries a state
 */
data class NonPositive<A>(val a: A) : NonPositiveOf<A> {
    companion object
}

@extension
interface NonPositiveFunctor : Functor<ForNonPositive> {
    override fun <A, B> Kind<ForNonPositive, A>.map(f: (A) -> B): Kind<ForNonPositive, B> =
        NonPositive(f(fix().a))
}

@extension
interface NonPositiveEq<A> : Eq<NonPositive<A>> {
    fun EQA(): Eq<A>
    override fun NonPositive<A>.eqv(b: NonPositive<A>): Boolean = EQA().run {
        a.eqv(b.a)
    }
}

@extension
interface NonPositiveShow<A> : Show<NonPositive<A>> {
    fun SA(): Show<A>
    override fun NonPositive<A>.show(): String = SA().run { a.show() }
}

@extension
interface NonPositiveArbitrary<A : Number> : Arbitrary<NonPositive<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<NonPositive<A>> =
        AA().arbitrary().suchThat { it.toDouble() <= 0 }.map { NonPositive(it) }

    override fun shrink(fail: NonPositive<A>): Sequence<NonPositive<A>> =
        AA().shrink(fail.a).filter { it.toDouble() <= 0 }
            .map { NonPositive(it) }
}

// ------------ ASCII strings
data class ASCIIString(val a: String) {
    companion object
}

@extension
interface ASCIIStringEq : Eq<ASCIIString> {
    override fun ASCIIString.eqv(b: ASCIIString): Boolean = String.eq().run {
        a.eqv(b.a)
    }
}

@extension
interface ASCIIStringOrder : Order<ASCIIString> {
    override fun ASCIIString.compare(b: ASCIIString): Int = String.order().run {
        a.compare(b.a)
    }
}

@extension
interface ASCIIStringShow : Show<ASCIIString> {
    override fun ASCIIString.show(): String = a
}

@extension
interface ASCIIStringArbitrary : Arbitrary<ASCIIString> {
    override fun arbitrary(): Gen<ASCIIString> = String.arbitrary().arbitrary().map {
        ASCIIString(
            it
        )
    }
    override fun shrink(fail: ASCIIString): Sequence<ASCIIString> = String.arbitrary().shrink(fail.a).map {
        ASCIIString(it)
    }
}

// ------------ Unicode strings
data class UnicodeString(val a: String) {
    companion object
}

@extension
interface UnicodeStringEq : Eq<UnicodeString> {
    override fun UnicodeString.eqv(b: UnicodeString): Boolean = String.eq().run {
        a.eqv(b.a)
    }
}

@extension
interface UnicodeStringOrder : Order<UnicodeString> {
    override fun UnicodeString.compare(b: UnicodeString): Int = String.order().run {
        a.compare(b.a)
    }
}

@extension
interface UnicodeStringShow : Show<UnicodeString> {
    override fun UnicodeString.show(): String = a
}

@extension
interface UnicodeStringArbitrary : Arbitrary<UnicodeString> {
    override fun arbitrary(): Gen<UnicodeString> = arbitraryUnicodeString().map {
        UnicodeString(
            it
        )
    }
    override fun shrink(fail: UnicodeString): Sequence<UnicodeString> = String.arbitrary().shrink(fail.a).map {
        UnicodeString(it)
    }
}