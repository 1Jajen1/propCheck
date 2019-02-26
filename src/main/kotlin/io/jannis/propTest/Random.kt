package io.jannis.propTest

import arrow.core.Tuple2
import arrow.core.toT

interface Random<A> {
    fun randomR(range: Tuple2<A, A>, seed: Long): Tuple2<A, Long>
    fun random(seed: Long): Tuple2<A, Long>
}

interface IntRandom : Random<Int> {
    override fun randomR(range: Tuple2<Int, Int>, seed: Long): Tuple2<Int, Long> =
        kotlin.random.Random(seed).let {
            if (range.a == range.b) range.a toT seed
            else it.nextInt(range.a, range.b) toT it.nextLong()
        }

    override fun random(seed: Long): Tuple2<Int, Long> =
        kotlin.random.Random(seed).let {
            it.nextInt() toT it.nextLong()
        }
}

fun Int.Companion.random(): Random<Int> = object : IntRandom {}

interface LongRandom : Random<Long> {
    override fun randomR(range: Tuple2<Long, Long>, seed: Long): Tuple2<Long, Long> =
        kotlin.random.Random(seed).let {
            if (range.a == range.b) range.a toT seed
            it.nextLong(range.a, range.b) toT it.nextLong()
        }

    override fun random(seed: Long): Tuple2<Long, Long> =
        kotlin.random.Random(seed).let {
            it.nextLong() toT it.nextLong()
        }
}

fun Long.Companion.random(): Random<Long> = object : LongRandom {}

interface FloatRandom : Random<Float> {
    override fun randomR(range: Tuple2<Float, Float>, seed: Long): Tuple2<Float, Long> =
        kotlin.random.Random(seed).let {
            if (range.a == range.b) range.a toT seed
            val diff = range.b - range.a
            (it.nextFloat() * diff + range.a) toT it.nextLong()
        }

    override fun random(seed: Long): Tuple2<Float, Long> =
        kotlin.random.Random(seed).let {
            it.nextFloat() toT it.nextLong()
        }
}

fun Float.Companion.random(): Random<Float> = object : FloatRandom {}

interface DoubleRandom : Random<Double> {
    override fun randomR(range: Tuple2<Double, Double>, seed: Long): Tuple2<Double, Long> =
        kotlin.random.Random(seed).let {
            if (range.a == range.b) range.a toT seed
            it.nextDouble(range.a, range.b) toT it.nextLong()
        }

    override fun random(seed: Long): Tuple2<Double, Long> =
        kotlin.random.Random(seed).let {
            it.nextDouble() toT it.nextLong()
        }
}

fun Double.Companion.random(): Random<Double> = object : DoubleRandom {}

interface BooleanRandom : Random<Boolean> {
    override fun randomR(range: Tuple2<Boolean, Boolean>, seed: Long): Tuple2<Boolean, Long> =
        kotlin.random.Random(seed).let {
            it.nextBoolean() toT it.nextLong()
        }

    override fun random(seed: Long): Tuple2<Boolean, Long> =
        kotlin.random.Random(seed).let {
            it.nextBoolean() toT it.nextLong()
        }
}

fun Boolean.Companion.random(): Random<Boolean> = object : BooleanRandom {}

interface CharRandom : Random<Char> {
    override fun randomR(range: Tuple2<Char, Char>, seed: Long): Tuple2<Char, Long> =
        kotlin.random.Random(seed).let {
            if (range.a == range.b) range.a toT seed
            it.nextInt(range.a.toInt(), range.b.toInt()).toChar() toT it.nextLong()
        }

    override fun random(seed: Long): Tuple2<Char, Long> =
        kotlin.random.Random(seed).let {
            it.nextInt(Char.MIN_VALUE.toInt(), Char.MAX_VALUE.toInt()).toChar() toT it.nextLong()
        }
}

fun Char.Companion.random(): Random<Char> = object : CharRandom {}