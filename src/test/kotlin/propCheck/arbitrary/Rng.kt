package propCheck.arbitrary

import arrow.core.Eval
import arrow.core.toT
import propCheck.*
import java.util.*

class RngSpec : PropertySpec({
    "same seed == same result"(Args(maxSuccess = 10000)) {
        forAll { (l, i): Pair<Long, Positive<Int>> ->
            (0..i.a).fold(true toT (RandSeed(l) toT RandSeed(l))) { (b, seeds), _ ->
                val resA = seeds.a.nextLong()
                val resB = seeds.b.nextLong()
                (b && resA.a == resB.a) toT (resA.b toT resB.b)
            }.a
        }
    }

    "same seed == same result with SplittableRandom (long)"(Args(maxSuccess = 10000)) {
        forAll { (l, i): Pair<Long, Positive<Int>> ->
            (0..i.a).fold(true toT (RandSeed(l) toT SplittableRandom(l))) { (b, seeds), _ ->
                val resA = seeds.a.nextLong()
                val resB = seeds.b.nextLong()
                (b && resA.a == resB) toT (resA.b toT seeds.b)
            }.a
        }
    }

    "same seed == same result with SplittableRandom (int)"(Args(maxSuccess = 10000)) {
        forAll { (l, i): Pair<Long, Positive<Int>> ->
            (0..i.a).fold(true toT (RandSeed(l) toT SplittableRandom(l))) { (b, seeds), _ ->
                val resA = seeds.a.nextInt()
                val resB = seeds.b.nextInt()
                (b && resA.a == resB) toT (resA.b toT seeds.b)
            }.a
        }
    }

    "same seed == same result with SplittableRandom (double)"(Args(maxSuccess = 10000)) {
        forAll { (l, i): Pair<Long, Positive<Int>> ->
            (0..i.a).fold(true toT (RandSeed(l) toT SplittableRandom(l))) { (b, seeds), _ ->
                val resA = seeds.a.nextDouble()
                val resB = seeds.b.nextDouble()
                (b && resA.a == resB) toT (resA.b toT seeds.b)
            }.a
        }
    }

    "same seed == same result with SplittableRandom (long ranged)"(Args(maxSuccess = 10000)) {
        forAll { l: Long ->
            forAll { (a, b): Pair<Long, Long> ->
                discardIf(
                    a >= b,
                    Eval.later {
                        (RandSeed(l).nextLong(a, b).a == SplittableRandom(l).nextLong(a, b)).property()
                    }
                )
            }
        }
    }

    "same seed == same result with SplittableRandom (int ranged)"(Args(maxSuccess = 10000)) {
        forAll { l: Long ->
            forAll { (a, b): Pair<Int, Int> ->
                discardIf(
                    a >= b,
                    Eval.later {
                        (RandSeed(l).nextInt(a, b).a == SplittableRandom(l).nextInt(a, b)).property()
                    }
                )
            }
        }
    }

    "same seed == same result with SplittableRandom (double ranged)"(Args(maxSuccess = 10000)) {
        forAll { l: Long ->
            forAll { (a, b): Pair<Double, Double> ->
                discardIf(
                    a >= b,
                    Eval.later {
                        (RandSeed(l).nextDouble(a, b).a == SplittableRandom(l).nextDouble(a, b)).property()
                    }
                )
            }
        }
    }

    "same seed == same result with SplittableRandom (after split)"(Args(maxSuccess = 10000)) {
        forAll { l: Long ->
            RandSeed(l).split().let { (a, b) ->
                val old = SplittableRandom(l)
                val new = old.split()

                a.nextLong().a == old.nextLong() &&
                        b.nextLong().a == new.nextLong()
            }
        }
    }
})