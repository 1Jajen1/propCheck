package io.jannis.propTest.instances

import arrow.core.toT
import arrow.data.ListK
import io.jannis.propTest.*

interface IntArbitrary : Arbitrary<Int> {
    override fun arbitrary(): Gen<Int> = arbitrarySizedInt()
    override fun shrink(fail: Int): Sequence<Int> = shrinkInt(fail)
}

fun Int.Companion.arbitrary(): Arbitrary<Int> = object : IntArbitrary {}

interface LongArbitrary : Arbitrary<Long> {
    override fun arbitrary(): Gen<Long> = arbitrarySizedLong()
    override fun shrink(fail: Long): Sequence<Long> = shrinkLong(fail)
}

fun Long.Companion.arbitrary(): Arbitrary<Long> = object :
    LongArbitrary {}

interface FloatArbitrary : Arbitrary<Float> {
    override fun arbitrary(): Gen<Float> = arbitrarySizedFloat()
    override fun shrink(fail: Float): Sequence<Float> = shrinkFloat(fail)
}

fun Float.Companion.arbitrary(): Arbitrary<Float> = object :
    FloatArbitrary {}

interface DoubleArbitrary : Arbitrary<Double> {
    override fun arbitrary(): Gen<Double> = arbitrarySizedDouble()
    override fun shrink(fail: Double): Sequence<Double> = shrinkDouble(fail)
}

fun Double.Companion.arbitrary(): Arbitrary<Double> = object :
    DoubleArbitrary {}

interface BooleanArbitrary : Arbitrary<Boolean> {
    override fun arbitrary(): Gen<Boolean> = Gen.elements(true, false)
    override fun shrink(fail: Boolean): Sequence<Boolean> = if (fail) sequenceOf(false) else emptySequence()
}

fun Boolean.Companion.arbitrary(): Arbitrary<Boolean> = object :
    BooleanArbitrary {}

interface CharArbitrary : Arbitrary<Char> {
    override fun arbitrary(): Gen<Char> = Gen.frequency(
        3 toT arbitraryASCIIChar(),
        1 toT arbitraryUnicodeChar()
    )

    override fun shrink(fail: Char): Sequence<Char> = (
            sequenceOf(
                'a', 'b', 'c'
            ) + (if (fail.isUpperCase()) sequenceOf(fail.toLowerCase()) else emptySequence()) +
                    sequenceOf('A', 'B', 'C') +
                    sequenceOf('1', '2', '3') +
                    sequenceOf(' ', '\n')
            ).filter {
        // TODO I don't really know if this is ported correctly
        it.isLowerCase().not() < fail.isLowerCase().not() ||
                it.isUpperCase().not() < fail.isUpperCase().not() ||
                it.isDigit().not() < fail.isDigit().not() ||
                (it == ' ').not() < (fail == ' ').not() ||
                it.isWhitespace().not() < fail.isWhitespace().not() ||
                it < fail
    }
}

fun Char.Companion.arbitrary(): Arbitrary<Char> = object :
    CharArbitrary {}

// not really a prim but included for simplicity
interface StringArbitrary : Arbitrary<String> {
    override fun arbitrary(): Gen<String> = arbitraryASCIIString()
    override fun shrink(fail: String): Sequence<String> = shrinkMap({
        it.toCharArray().toList().k()
    }, {
        it.joinToString("")
    }, ListK.arbitrary(Char.arbitrary())).invoke(fail)
}

fun String.Companion.arbitrary(): Arbitrary<String> = object :
    StringArbitrary {}