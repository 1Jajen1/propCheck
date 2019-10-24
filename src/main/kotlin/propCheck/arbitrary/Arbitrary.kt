package propCheck.arbitrary

import arrow.core.Tuple2
import arrow.core.extensions.list.foldable.foldLeft
import arrow.core.toT
import arrow.optics.Iso
import propCheck.arbitrary.gen.monad.monad
import propCheck.instances.arbitrary

/**
 * Arbitrary interface
 * Minimal complete definition is just arbitrary() for which there are plenty of combinators to implement
 */
interface Arbitrary<A> {
    /**
     * Create a generator capable of producing A's
     */
    fun arbitrary(): Gen<A>

    /**
     * shrink an A to a Sequence of A's
     */
    fun shrink(fail: A): Sequence<A> = emptySequence()

    companion object {
        operator fun <A> invoke(g: Gen<A>, shrinkA: (A) -> Sequence<A> = { emptySequence() }): Arbitrary<A> =
            object : Arbitrary<A> {
                override fun arbitrary(): Gen<A> = g
                override fun shrink(fail: A): Sequence<A> = shrinkA.invoke(fail)
            }
    }
}

// ----------------------- Helpers for numbers
// ---------- Number generators respecting the size parameter
fun arbitrarySizedInt(): Gen<Int> =
    Gen.sized { Gen.choose(-it toT it, Int.random()) }

fun arbitrarySizedLong(): Gen<Long> = Gen.sized {
    Gen.choose(
        -it.toLong() toT it.toLong(),
        Long.random()
    )
}

fun arbitrarySizedPositiveInt(): Gen<Int> =
    Gen.sized { Gen.choose(0 toT it, Int.random()) }

fun arbitrarySizedPositiveLong(): Gen<Long> = Gen.sized {
    Gen.choose(
        0L toT it.toLong(),
        Long.random()
    )
}

fun arbitrarySizedFloat(): Gen<Float> = Gen.sized { n ->
    Gen.monad().fx.monad {
        val b = Gen.choose(1L toT 999999999L, Long.random()).bind()
        val a = Gen.choose((-n) * b toT n * b, Long.random()).bind()
        a.toFloat() / b.toFloat()
    }.fix()
}

fun arbitrarySizedDouble(): Gen<Double> = Gen.sized { n ->
    Gen.monad().fx.monad {
        val b = Gen.choose(1L toT 999999999L, Long.random()).bind()
        val a = Gen.choose((-n) * b toT n * b, Long.random()).bind()
        a.toDouble() / b.toDouble()
    }.fix()
}

fun arbitrarySizedByte(): Gen<Byte> = Gen.sized { n ->
    Gen.choose(
        when {
            -n < Byte.MIN_VALUE -> Byte.MIN_VALUE
            else -> (-n).toByte()
        } toT when {
            n > Byte.MAX_VALUE -> Byte.MAX_VALUE
            else -> n.toByte()
        },
        Byte.random()
    )
}

// ------------- Number generators ignoring the size parameter, will go for entire range of type
fun arbitraryBoundedInt(): Gen<Int> = Gen.chooseAny(Int.random())

fun arbitraryBoundedLong(): Gen<Long> = Gen.chooseAny(Long.random())
fun arbitraryBoundedByte(): Gen<Byte> = Gen.chooseAny(Byte.random())

fun arbitraryBoundedFloat(): Gen<Float> =
    Gen.chooseAny(Float.random())

fun arbitraryBoundedDouble(): Gen<Double> =
    Gen.chooseAny(Double.random())

// ----------- String and char generators
fun arbitraryASCIIChar(): Gen<Char> =
    Gen.choose(32.toChar() toT 127.toChar(), Char.random())

fun arbitraryUnicodeChar(): Gen<Char> = Gen.chooseAny(Char.random()).suchThat { !it.isSurrogate() }

fun arbitraryUnicodeString(): Gen<String> = arbitraryUnicodeChar().listOf().map { it.joinToString("") }

fun arbitraryASCIIString(): Gen<String> = arbitraryASCIIChar().listOf().map { it.joinToString("") }

// ---------------------------------- helpers to create shrink functions
/**
 * implement a shrink function by mapping back and forth between a type that already has a shrink function defined
 */
fun <A, B> shrinkMap(f: (A) -> B, g: (B) -> A, arbA: Arbitrary<B>): (A) -> Sequence<A> = { fail ->
    arbA.shrink(f(fail)).map(g)
}

/**
 * implement a shrink function using an isomorphism from arrow-optics and an Arbitrary<B>
 */
fun <A, B> shrinkMap(iso: Iso<A, B>, arbB: Arbitrary<B>): (A) -> Sequence<A> = { fail ->
    arbB.shrink(iso.get(fail)).map { iso.reverseGet(it) }
}

/**
 * shrink a list (by shrinking recursively, shrinking its content and shrinking the list size itself)
 */
fun <A> shrinkList(list: List<A>, f: (A) -> Sequence<A>): Sequence<List<A>> {
    fun <F> removes(k: Int, n: Int, l: List<F>): Sequence<List<F>> =
        if (k > n) emptySequence()
        else if (l.isEmpty()) sequenceOf(emptyList())
        else sequenceOf(l.drop(k)) + sequenceOf(Unit).flatMap { removes(k, (n - k), l.drop(k)).map { l.take(k) + it } }

    fun shrinkListIt(l: List<A>): Sequence<List<A>> = when (l.size) {
        0 -> emptySequence()
        else -> iterate({ it / 2 }, l.size).takeWhile { it > 0 }
            .map { k -> removes(k, l.size, l) }.reduce { a, b -> a + b }
    }

    fun shrinkOne(l: List<A>): Sequence<List<A>> = when (l.size) {
        0 -> emptySequence()
        else -> f(l[0]).map { listOf(it) + l.drop(1) } +
                shrinkOne(l.drop(1)).map { listOf(l[0]) + it }
    }

    return if (list.isEmpty()) emptySequence()
    else shrinkListIt(list) + sequenceOf(Unit).flatMap { shrinkOne(list) }
}

// ---------------------------- Number shrinkers
fun shrinkByte(fail: Byte): Sequence<Byte> = (
        (if (fail < 0 && (-fail).toByte() > fail) sequenceOf(-fail) else emptySequence()) +
                (sequenceOf(0) + iterate({ it / 2 }, fail.toInt()).drop(1)
                    .map { fail - it }).takeWhile {
                    when ((it >= 0) toT (fail >= 0)) {
                        Tuple2(true, true) -> it < fail
                        Tuple2(false, false) -> it > fail
                        Tuple2(true, false) -> (it + fail) < 0
                        Tuple2(false, true) -> (it + fail) > 0
                        else -> throw IllegalStateException("The impossible happened")
                    }
                }
        ).map { it.toByte() } // safe because shrinking only makes things smaller and we start with a byte anyway

fun shrinkInt(fail: Int): Sequence<Int> = (
        (if (fail < 0 && -fail > fail) sequenceOf(-fail) else emptySequence()) +
                (sequenceOf(0) + iterate({ it / 2 }, fail).drop(1)
                    .map { fail - it }).takeWhile {
                    when ((it >= 0) toT (fail >= 0)) {
                        Tuple2(true, true) -> it < fail
                        Tuple2(false, false) -> it > fail
                        Tuple2(true, false) -> (it + fail) < 0
                        Tuple2(false, true) -> (it + fail) > 0
                        else -> throw IllegalStateException("The impossible happened")
                    }
                }
        )

fun shrinkLong(fail: Long): Sequence<Long> = (
        (if (fail < 0 && -fail > fail) sequenceOf(-fail) else emptySequence()) +
                (sequenceOf(0L) + iterate({ it / 2 }, fail).drop(1).map { fail - it }).takeWhile {
                    when ((it >= 0) toT (fail >= 0)) {
                        Tuple2(true, true) -> it < fail
                        Tuple2(false, false) -> it > fail
                        Tuple2(true, false) -> (it + fail) < 0
                        Tuple2(false, true) -> (it + fail) > 0
                        else -> throw IllegalStateException("The impossible happened")
                    }
                }
        )

fun shrinkFloat(fail: Float): Sequence<Float> = (
        (when (fail) {
            Float.NaN, Float.NEGATIVE_INFINITY, Float.POSITIVE_INFINITY -> sequenceOf(0F)
            else -> emptySequence()
        }) +
                (iterate({ it * 10 }, 1).take(6).map { precision ->
                    Math.round(fail * precision) toT precision
                }
                    .filter { (m, _) -> m.rem(10) != 0 }
                    .map { (m, p) -> (sequenceOf(m) + Int.arbitrary().shrink(m)) toT p }
                    .flatMap { (l, p) ->
                        l.map { it / p.toFloat() }.filter { Math.abs(it) < Math.abs(fail) }
                    }
                        )
        )

fun shrinkDouble(fail: Double): Sequence<Double> = (
        (when (fail) {
            Double.NaN, Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY -> sequenceOf(0.toDouble())
            else -> emptySequence()
        }) + (iterate({ it * 10 }, 1).take(6).map { precision ->
            Math.round(fail * precision) toT precision
        }
            .filter { (m, _) -> m.rem(10) != 0L }
            .map { (m, p) -> (sequenceOf(m) + Long.arbitrary().shrink(m)) toT p }
            .flatMap { (l, p) ->
                l.map { it / p.toDouble() }.filter { Math.abs(it) < Math.abs(fail) }
            }
                )
        )

fun shrinkChar(fail: Char): Sequence<Char> = (
        sequenceOf(
            'a', 'b', 'c'
        ) + (if (fail.isUpperCase()) sequenceOf(fail.toLowerCase()) else emptySequence()) +
                sequenceOf('A', 'B', 'C') +
                sequenceOf('1', '2', '3') +
                sequenceOf(' ', '\n')
        ).filter {
    listOf(
        (it.isLowerCase().not().compareTo(fail.isLowerCase().not())),
        (it.isUpperCase().not().compareTo(fail.isUpperCase().not())),
        (it.isDigit().not().compareTo(fail.isDigit().not())),
        ((it == ' ').not().compareTo((fail == ' ').not())),
        (it.isWhitespace().not().compareTo(fail.isWhitespace().not())),
        (it.toInt().compareTo(fail.toInt()))
    ).foldLeft(0) { acc, i ->
        if (acc != 0) acc
        else when {
            i > 0 -> 1
            i == 0 -> acc
            else -> i
        }
    } < 0

}.also {
    val t = it.toList()
    println(t)
}


fun <T : Any> iterate(f: (T) -> T, start: T) = generateSequence(start) { f(it) }