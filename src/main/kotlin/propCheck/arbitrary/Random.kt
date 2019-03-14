package propCheck.arbitrary

import arrow.core.Tuple2
import arrow.core.toT

/**
 * Random typeclass similar to haskell and instances for primitive types
 */
interface Random<A> {
    fun randomR(range: Tuple2<A, A>, seed: RandSeed): Tuple2<A, RandSeed>
    fun random(seed: RandSeed): Tuple2<A, RandSeed>
}

interface IntRandom : Random<Int> {
    override fun randomR(range: Tuple2<Int, Int>, seed: RandSeed): Tuple2<Int, RandSeed> =
        if (range.a == range.b) range.a toT seed
        else seed.nextInt(range.a, range.b)

    override fun random(seed: RandSeed): Tuple2<Int, RandSeed> =
        seed.nextInt()
}

fun Int.Companion.random(): Random<Int> = object : IntRandom {}

interface LongRandom : Random<Long> {
    override fun randomR(range: Tuple2<Long, Long>, seed: RandSeed): Tuple2<Long, RandSeed> =
        if (range.a == range.b) range.a toT seed
        else seed.nextLong(range.a, range.b)

    override fun random(seed: RandSeed): Tuple2<Long, RandSeed> =
        seed.nextLong()
}

fun Long.Companion.random(): Random<Long> = object : LongRandom {}

interface FloatRandom : Random<Float> {
    override fun randomR(range: Tuple2<Float, Float>, seed: RandSeed): Tuple2<Float, RandSeed> =
        if (range.a == range.b) range.a toT seed
        else seed.nextDouble().let { (a, s) ->
            (a * (range.b.toDouble() - range.a.toDouble()) + range.a.toDouble()).toFloat() toT s
        }

    override fun random(seed: RandSeed): Tuple2<Float, RandSeed> =
        seed.nextDouble(Float.MIN_VALUE.toDouble(), Float.MAX_VALUE.toDouble()).let { (a, s) ->
            a.toFloat() toT s
        }
}

fun Float.Companion.random(): Random<Float> = object : FloatRandom {}

interface DoubleRandom : Random<Double> {
    override fun randomR(range: Tuple2<Double, Double>, seed: RandSeed): Tuple2<Double, RandSeed> =
        if (range.a == range.b) range.a toT seed
        else seed.nextDouble(range.a, range.b)

    override fun random(seed: RandSeed): Tuple2<Double, RandSeed> =
        seed.nextDouble()
}

fun Double.Companion.random(): Random<Double> = object : DoubleRandom {}

interface ByteRandom : Random<Byte> {
    override fun random(seed: RandSeed): Tuple2<Byte, RandSeed> =
        seed.nextInt(Byte.MIN_VALUE.toInt(), Byte.MAX_VALUE.toInt()).let { (a, s) ->
            a.toByte() toT s
        }

    override fun randomR(range: Tuple2<Byte, Byte>, seed: RandSeed): Tuple2<Byte, RandSeed> =
        if (range.a == range.b) range.a toT seed
        else seed.nextInt(range.a.toInt(), range.b.toInt()).let { (a, s) ->
            a.toByte() toT s
        }
}

fun Byte.Companion.random(): Random<Byte> = object : ByteRandom {}

interface BooleanRandom : Random<Boolean> {
    override fun randomR(range: Tuple2<Boolean, Boolean>, seed: RandSeed): Tuple2<Boolean, RandSeed> =
            seed.nextInt(0, 1).let { (a, s) ->
                (a == 0) toT s
            }

    override fun random(seed: RandSeed): Tuple2<Boolean, RandSeed> =
        seed.nextInt(0, 2).let { (a, s) ->
            (a == 0) toT s
        }
}

fun Boolean.Companion.random(): Random<Boolean> = object : BooleanRandom {}

interface CharRandom : Random<Char> {
    override fun randomR(range: Tuple2<Char, Char>, seed: RandSeed): Tuple2<Char, RandSeed> =
            if (range.a == range.b) range.a toT seed
            else seed.nextInt(range.a.toInt(), range.b.toInt()).let { (a, s) ->
                a.toChar() toT s
            }

    override fun random(seed: RandSeed): Tuple2<Char, RandSeed> =
            seed.nextInt(Char.MIN_VALUE.toInt(), Char.MAX_VALUE.toInt()).let { (a, s) ->
                a.toChar() toT s
            }
}

fun Char.Companion.random(): Random<Char> = object : CharRandom {}