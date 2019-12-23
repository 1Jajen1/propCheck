package propCheck.arbitrary

import arrow.core.Tuple2
import arrow.core.andThen
import arrow.core.toT

data class Range<A>(val origin: A, val bounds: (Int) -> Tuple2<A, A>) {

    fun <B>map(f: (A) -> B): Range<B> = Range(
        f(origin),
        bounds andThen { (a, b) -> f(a) toT f(b) }
    )

    fun lowerBound(s: Int): A = bounds(s).a

    fun upperBound(s: Int): A = bounds(s).b

    companion object {
        fun <A>constant(start: A, end: A): Range<A> = Range(start) { start toT end }

        fun constant(range: IntRange): Range<Int> = Range(range.first) { range.first toT range.last }
        fun constant(range: CharRange): Range<Char> = Range(range.first) { range.first toT range.last }
        fun constant(range: LongRange): Range<Long> = Range(range.first) { range.first toT range.last }

        fun range(range: IntRange): Range<Int> = Range(range.first) { range.first toT (range.first + range.step * ((range.last - range.first) / range.step)) }

        /*
        last - first <= step * x
        (last - first) / step <= x

        150 20 50 = 20 70 120 = 3 -.- So I want both, but 120 only after 2/3 of tries, but never any other results
        0-2 should be my it but I have 0-99.

        first * step * (it * (last - first) / step))

        stepMult = (ceil((last - first) / step).toDouble() / 100)
        first * step * floor(it * stepMult) // braces to keep precision
         */
    }
}