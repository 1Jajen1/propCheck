package propCheck.arbitrary

import arrow.core.toT

fun Long.shrinkTowards(destination: Long): Sequence<Long> = when (destination) {
    this -> emptySequence()
    else -> {
        val diff = (this / 2) - (destination / 2)
        sequenceOf(destination) + halves(diff).map { this - it }
    }
}

fun Double.shrinkTowards(destination: Double): Sequence<Double> = when (destination) {
    this -> emptySequence()
    else -> {
        val diff = this - destination
        val ok = { d: Double -> d != this && d.isNaN().not() && d.isInfinite().not() }
        sequenceOf(destination) + iterate(diff) { it / 2 }
            .map { this - it }
            .takeWhile(ok)
    }
}

fun <A> List<A>.shrink(): Sequence<List<A>> = halves(size.toLong())
    .flatMap { removes(it.toInt()) }

fun <A> List<A>.removes(n: Int): Sequence<List<A>> = loopRemove(n, size)

private fun <A>List<A>.loopRemove(k: Int, n: Int): Sequence<List<A>> =
    (take(k) toT drop(k)).let { (head, tail) ->
        when {
            k > n -> emptySequence()
            tail.isEmpty() -> sequenceOf(emptyList())
            else -> sequenceOf(tail) + sequenceOf(Unit).flatMap {
                tail.loopRemove(k, n - k).map { head + it }
            }
        }
    }

fun <T : Any> iterate(start: T, f: (T) -> T) = generateSequence(start) { f(it) }

fun halves(i: Long): Sequence<Long> =
    generateSequence(i) { it / 2 }.takeWhile { it != 0L }

