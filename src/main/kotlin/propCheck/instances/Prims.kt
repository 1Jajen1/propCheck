package propCheck.instances

import arrow.core.toT
import arrow.data.ListK
import arrow.data.k
import propCheck.arbitrary.*
import propCheck.arbitrary.gen.monad.monad
import propCheck.instances.listk.arbitrary.arbitrary

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

interface ByteArbitrary : Arbitrary<Byte> {
    override fun arbitrary(): Gen<Byte> = arbitrarySizedByte()
    override fun shrink(fail: Byte): Sequence<Byte> = shrinkByte(fail)
}

fun Byte.Companion.arbitrary(): Arbitrary<Byte> = object : ByteArbitrary {}

interface CharArbitrary : Arbitrary<Char> {
    override fun arbitrary(): Gen<Char> = Gen.frequency(
        3 toT arbitraryASCIIChar(),
        1 toT arbitraryUnicodeChar()
    )

    override fun shrink(fail: Char): Sequence<Char> = shrinkChar(fail)
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

// ---------- arrays
val intArrayArb = object : Arbitrary<IntArray> {
    val intArb = Int.arbitrary()
    override fun arbitrary(): Gen<IntArray> = Gen.sized {
        Gen.monad().binding {
            IntArray(it) { intArb.arbitrary().bind() }
        }.fix()
    }

    override fun shrink(fail: IntArray): Sequence<IntArray> = shrinkMap({
        it.toList().k()
    }, { l ->
        IntArray(l.size) { l[it] }
    }, ListK.arbitrary(Int.arbitrary())).invoke(fail)
}

val longArrayArb = object : Arbitrary<LongArray> {
    val longArb = Long.arbitrary()
    override fun arbitrary(): Gen<LongArray> = Gen.sized {
        Gen.monad().binding {
            LongArray(it) { longArb.arbitrary().bind() }
        }.fix()
    }

    override fun shrink(fail: LongArray): Sequence<LongArray> = shrinkMap({
        it.toList().k()
    }, { l ->
        LongArray(l.size) { l[it] }
    }, ListK.arbitrary(Long.arbitrary())).invoke(fail)
}

val floatArrayArb = object : Arbitrary<FloatArray> {
    val floatArb = Float.arbitrary()
    override fun arbitrary(): Gen<FloatArray> = Gen.sized {
        Gen.monad().binding {
            FloatArray(it) { floatArb.arbitrary().bind() }
        }.fix()
    }

    override fun shrink(fail: FloatArray): Sequence<FloatArray> = shrinkMap({
        it.toList().k()
    }, { l ->
        FloatArray(l.size) { l[it] }
    }, ListK.arbitrary(Float.arbitrary())).invoke(fail)
}

val doubleArrayArb = object : Arbitrary<DoubleArray> {
    val doubleArb = Double.arbitrary()
    override fun arbitrary(): Gen<DoubleArray> = Gen.sized {
        Gen.monad().binding {
            DoubleArray(it) { doubleArb.arbitrary().bind() }
        }.fix()
    }

    override fun shrink(fail: DoubleArray): Sequence<DoubleArray> = shrinkMap({
        it.toList().k()
    }, { l ->
        DoubleArray(l.size) { l[it] }
    }, ListK.arbitrary(Double.arbitrary())).invoke(fail)
}

val byteArrayArb = object : Arbitrary<ByteArray> {
    val byteArb = Byte.arbitrary()
    override fun arbitrary(): Gen<ByteArray> = Gen.sized {
        Gen.monad().binding {
            ByteArray(it) { byteArb.arbitrary().bind() }
        }.fix()
    }

    override fun shrink(fail: ByteArray): Sequence<ByteArray> = shrinkMap({
        it.toList().k()
    }, { l ->
        ByteArray(l.size) { l[it] }
    }, ListK.arbitrary(Byte.arbitrary())).invoke(fail)
}

val booleanArrayArb = object : Arbitrary<BooleanArray> {
    val booleanArb = Boolean.arbitrary()
    override fun arbitrary(): Gen<BooleanArray> = Gen.sized {
        Gen.monad().binding {
            BooleanArray(it) { booleanArb.arbitrary().bind() }
        }.fix()
    }

    override fun shrink(fail: BooleanArray): Sequence<BooleanArray> = shrinkMap({
        it.toList().k()
    }, { l ->
        BooleanArray(l.size) { l[it] }
    }, ListK.arbitrary(Boolean.arbitrary())).invoke(fail)
}

fun <A> arrayArb(aA: Arbitrary<A>): Arbitrary<Array<A>> = object :
    Arbitrary<Array<A>> {
    override fun arbitrary(): Gen<Array<A>> = ListK.arbitrary(aA).arbitrary().map { l ->
        Array<Any?>(l.size) { l[it] } as Array<A>
    }

    override fun shrink(fail: Array<A>): Sequence<Array<A>> = shrinkMap({
        it.toList().k()
    }, { l ->
        Array<Any?>(l.size) { l[it] } as Array<A>
    }, ListK.arbitrary(aA)).invoke(fail)
}