package propCheck.arbitrary

import arrow.core.Tuple2
import arrow.core.some
import arrow.core.toT
import propCheck.*
import java.util.*

/**
 * This whole thing is copypasta simply to make SplittableRandom more accessible
 * re-implementation of SplittableRandom
 *  by http://gee.cs.oswego.edu/dl/papers/oopsla14.pdf
 *
 * Why? Because all other random generators hide the seed in a private field and
 *  I don't want to rely on reflection to get it!
*/
class RandSeed private constructor(
    val seed: Long,
    val gamma: Long // Has to be odd!
) {

    override fun toString(): String = "RandSeed($seed, $gamma)"

    fun nextLong(): Tuple2<Long, RandSeed> = nextSeed(seed, gamma).let { mix64(it) toT RandSeed(it, gamma) }
    fun nextLong(origin: Long, bound: Long): Tuple2<Long, RandSeed> = when {
        origin >= bound -> throw IllegalArgumentException("Invalid bounds $origin $bound")
        else -> {
            nextSeed(seed, gamma).let {
                var r = mix64(it)
                var latestSeed = it
                val n = bound - origin
                val m = n - 1
                if (n and m == 0L)
                    r = (r and m) + origin
                else if (n > 0) {
                    var u = r.ushr(1)
                    r = u.rem(n)
                    while (u + m - u.rem(n) < 0) {
                        r = u.rem(n)
                        latestSeed = nextSeed(latestSeed, gamma)
                        u = mix64(latestSeed).ushr(1)
                    }
                    r += origin
                } else {
                    while (r < origin || r >= bound) {
                        latestSeed = nextSeed(latestSeed, gamma)
                        r = mix64(latestSeed)
                    }
                }
                r toT RandSeed(latestSeed, gamma)
            }
        }
    }

    fun nextInt(): Tuple2<Int, RandSeed> = nextSeed(seed, gamma).let { mix32(it) toT RandSeed(it, gamma) }
    fun nextInt(origin: Int, bound: Int): Tuple2<Int, RandSeed> = when {
        origin >= bound -> throw IllegalArgumentException("Invalid bounds $origin $bound")
        else -> {
            nextSeed(seed, gamma).let {
                var r = mix32(it)
                var latestSeed = it
                val n = bound - origin
                val m = n - 1
                if (n and m == 0)
                    r = (r and m) + origin
                else if (n > 0) {
                    var u = r.ushr(1)
                    r = u.rem(n)
                    while (u + m - u.rem(n) < 0) {
                        r = u.rem(n)
                        latestSeed = nextSeed(latestSeed, gamma)
                        u = mix32(latestSeed).ushr(1)
                    }
                    r += origin
                } else {
                    while (r < origin || r >= bound) {
                        latestSeed = nextSeed(latestSeed, gamma)
                        r = mix32(latestSeed)
                    }
                }
                r toT RandSeed(latestSeed, gamma)
            }
        }
    }

    fun nextDouble(): Tuple2<Double, RandSeed> = nextSeed(seed, gamma).let {
        mix64(it).ushr(11) * DOUBLE_UNIT toT RandSeed(it, gamma)
    }
    fun nextDouble(origin: Double, bound: Double): Tuple2<Double, RandSeed> {
        val (l, s) = nextLong()
        var r = l.ushr(11) * DOUBLE_UNIT
        if (origin < bound) {
            r = r * (bound - origin) + origin
            if (r >= bound)
            // correct for rounding
            r = (bound.toLong() - 1).toDouble()
            // TODO Check if those two are equal!
            // r = java.lang.Double.longBitsToDouble(java.lang.Double.doubleToLongBits(bound) - 1)
        }
        return r toT s
    }

    fun split(): Tuple2<RandSeed, RandSeed> {
        val (l, seed) = nextLong()
        val nl = nextSeed(seed.seed, gamma)
        val nextGamma = mixGamma(nl)
        return RandSeed(nl, seed.gamma) toT RandSeed(l, nextGamma)
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as RandSeed

        if (seed != other.seed) return false
        if (gamma != other.gamma) return false

        return true
    }

    override fun hashCode(): Int {
        var result = seed.hashCode()
        result = 31 * result + gamma.hashCode()
        return result
    }

    companion object {

        operator fun invoke(seed: Long, gamma: Long = GOLDEN_GAMMA): RandSeed =
            if (gamma.rem(2L) == 0L) throw IllegalArgumentException("Gamma has to be an odd value!")
            else RandSeed(seed, gamma)

        internal const val GOLDEN_GAMMA = -0x61c8864680b583ebL
        private const val DOUBLE_UNIT = 1.0 / ((1L).shl(53))

        private fun nextSeed(seed: Long, gamma: Long): Long = seed + gamma

        private fun mix64(z: Long): Long {
            val z1 = (z xor z.ushr(30)) * -0x40a7b892e31b1a47L
            val z2 = (z1 xor z1.ushr(27)) * -0x6b2fb644ecceee15L
            return z2 xor z2.ushr(31)
        }

        private fun mix32(z: Long): Int {
            val z1 = (z xor z.ushr(33)) * 0x62a9d9ed799705f5L
            return ((z1 xor z1.ushr(28)) * -0x34db2f5a3773ca4dL).ushr(32).toInt()
        }

        private fun mixGamma(z: Long): Long {
            val z1 = (z xor z.ushr(33)) * -0xae502812aa7333L // MurmurHash3 mix constants
            val z2 = (z1 xor z1.ushr(33)) * -0x3b314601e57a13adL
            val z3 = z2 xor z2.ushr(33) or 1L                  // force to be odd
            val n = bitCount(z3 xor z3.ushr(1))       // ensure enough transitions
            return if (n < 24) z3 xor -0x5555555555555556L else z3
        }

        // java.lang.Long.bitcount
        private fun bitCount(var0: Long): Int {
            var var0 = var0
            var0 -= var0.ushr(1) and 6148914691236517205L
            var0 = (var0 and 3689348814741910323L) + (var0.ushr(2) and 3689348814741910323L)
            var0 = var0 + var0.ushr(4) and 1085102592571150095L
            var0 += var0.ushr(8)
            var0 += var0.ushr(16)
            var0 += var0.ushr(32)
            return var0.toInt() and 127
        }
    }
}