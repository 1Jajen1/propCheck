package io.jannis.propTest

import arrow.Kind
import arrow.core.*
import arrow.data.Nel
import arrow.data.extensions.list.traverse.sequence
import arrow.data.extensions.list.traverse.traverse
import arrow.data.fix
import arrow.data.k
import arrow.effects.IO
import arrow.effects.extensions.io.applicative.applicative
import arrow.effects.extensions.io.monad.monad
import arrow.effects.fix
import arrow.higherkind
import arrow.typeclasses.Applicative
import arrow.typeclasses.Functor
import arrow.typeclasses.Monad
import arrow.typeclasses.Show
import io.jannis.propTest.instances.arbitrary

@higherkind
class Gen<A>(val unGen: (Tuple2<Long, Int>) -> A) : GenOf<A> {

    fun <B> map(f: (A) -> B): Gen<B> = genMap(f)
    internal fun <B> genMap(f: (A) -> B): Gen<B> = Gen { f(fix().unGen(it)) }

    fun resize(i: Int): Gen<A> = if (i < 0)
        throw IllegalArgumentException("Size must be non-negative")
    else
        Gen { (r, _) -> this@Gen.unGen(r toT i) }

    fun scale(f: (Int) -> Int): Gen<A> = sized { resize(f(it)) }

    // combinators
    fun suchThat(pred: (A) -> Boolean): Gen<A> = Gen.monad().fx {
        suchThatOption(pred).bind().fold({
            sized { suchThat(pred).resize(it + 1) }.bind()
        }, {
            it
        })
    }.fix()

    fun suchThatOption(pred: (A) -> Boolean): Gen<Option<A>> =
        sized {
            fun attempt(m: Int, n: Int): Gen<Option<A>> = Gen.monad().fx {
                if (m > n) none()
                else {
                    val res = resize(m).bind()
                    if (pred(res)) res.some() else attempt(m + 1, n).bind()
                }
            }.fix()
            attempt(it, it * 2)
        }

    fun <B> suchThatMap(pred: (A) -> Option<B>): Gen<B> = map(pred).suchThat { it.isDefined() }
        .map { it.orNull()!! }

    fun listOf(): Gen<List<A>> = sized { n ->
        Gen.monad().fx {
            val k = choose(0 toT n, Int.random()).bind()
            this@Gen.vectorOf(k).bind()
        }.fix()
    }

    fun nelOf(): Gen<Nel<A>> = sized { n ->
        Gen.monad().fx {
            val k = choose(1 toT Math.max(1, n), Int.random()).bind()
            Nel.fromListUnsafe(this@Gen.vectorOf(k).bind())
        }.fix()
    }

    fun vectorOf(n: Int): Gen<List<A>> = Gen.monad().fx {
        (0..n).toList().map { this@Gen.bind() }
    }.fix()

    companion object {

        fun functor(): Functor<ForGen> = object: GenFunctor {}
        fun applicative(): Applicative<ForGen> = object: GenApplicative {}
        fun monad(): Monad<ForGen> = object: GenMonad {}

        fun <A> sized(f: (Int) -> Gen<A>): Gen<A> =
            Gen { (r, n) -> f(n).unGen(r toT n) }
        fun getSize(): Gen<Int> = Gen { (_, n) -> n }

        fun <A> choose(range: Tuple2<A, A>, randA: Random<A>): Gen<A> =
            Gen { (r, _) -> randA.randomR(range, r).a }

        fun <A> chooseAny(randA: Random<A>): Gen<A> =
            Gen { (r, _) -> randA.random(r).a }

        fun <A> oneOf(vararg gens: Gen<A>): Gen<A> = if (gens.size < 0)
            throw IllegalArgumentException("oneOf cannot work with 0 generators")
        else Gen.monad().fx {
            gens[choose(0 toT (gens.size - 1), Int.random()).bind()].bind()
        }.fix()

        fun <A> frequency(vararg gens: Tuple2<Int, Gen<A>>): Gen<A> = if (gens.size < 0)
            throw IllegalArgumentException("frequency cannot work with 0 generators")
        else Gen.monad().fx {
            val sum = gens.map { it.a }.sum() + 1 // + 1 for inclusive range
            val c = choose(0 toT sum, Int.random()).bind()

            fun pick(n: Int, l: List<Tuple2<Int, Gen<A>>>): Gen<A> {
                val (k, g) = l[0]
                return if (n <= k) g
                else pick(n - k, l.drop(1))
            }

            pick(c, gens.toList()).bind()
        }.fix()

        fun <A> elements(vararg el: A): Gen<A> = if (el.size < 0)
            throw IllegalArgumentException("elements cannot work without elements")
        else Gen.monad().fx {
            el[choose(0 toT (el.size - 1), Int.random()).bind()]
        }.fix()

        fun <A> sublistOf(list: List<A>): Gen<List<A>> = Gen.monad().fx {
            list.filter { chooseAny(Boolean.random()).bind() }
        }.fix()

        fun <A> shuffle(list: List<A>): Gen<List<A>> = Gen.monad().fx {
            val l = arbitraryBoundedInt().vectorOf(list.size).bind()
            list.zip(l).sortedBy { it.second }.map { it.first }
        }.fix()
    }

    fun generate(): IO<A> = IO {
        unGen(kotlin.random.Random.nextLong() toT 30)
    }

    fun sample(): IO<List<A>> = (1..20 step 2)
        .map { resize(it) }.map { it.generate() }.sequence(IO.applicative())
        .fix().map { it.fix() }

    fun classify(n: Int = 100, text: String, f: (A) -> Boolean): IO<Unit> = (1..n).map { generate() }
        .sequence(IO.applicative()).fix().flatMap {
            it.fix().foldLeft(0) { acc, v ->
                if (f(v)) acc + 1
                else acc
            }.let {
                IO { println("${"%.2f".format(100 * (it.toDouble() / n.toDouble()))}% $text") }
            }
        }

    fun tabulate(n: Int = 100, name: String, f: (A) -> String): IO<Unit> = (1..n).map { generate() }
        .sequence(IO.applicative()).fix().map {
            it.fix().fold(mutableMapOf<String, Int>()) { map, v ->
                f(v).let {
                    map.compute(it) { _, v -> (v ?: 0) + 1 }
                    map
                }
            }
        }.flatMap {
            IO.monad().fx {
                IO { println("$name:") }.bind()
                it.entries.toList().sortedBy { (_, v) -> v }.map { (k, v) -> k toT v }
                    .traverse(IO.applicative()) { (k, v) ->
                        IO { println("${"%.2f".format(100 * (v.toDouble() / n.toDouble()))}% $k") }
                    }.fix().flatMap { IO.unit }.bind()
            }.fix()
        }
}

// @extension
interface GenFunctor : Functor<ForGen> {
    override fun <A, B> Kind<ForGen, A>.map(f: (A) -> B): Kind<ForGen, B> = fix().genMap(f)
}

// @extension
interface GenApplicative : Applicative<ForGen> {
    override fun <A, B> Kind<ForGen, A>.ap(ff: Kind<ForGen, (A) -> B>): Kind<ForGen, B> =
        Gen.monad().run { ff.flatMap { f -> map(f) } }

    override fun <A> just(a: A): Kind<ForGen, A> =
        Gen { a }
}

// @extension
interface GenMonad : Monad<ForGen> {
    override fun <A, B> Kind<ForGen, A>.flatMap(f: (A) -> Kind<ForGen, B>): Kind<ForGen, B> =
        Gen { (r, n) ->
            f(
                fix().unGen(r toT n)
            ).fix().unGen(kotlin.random.Random(r).nextLong() toT n)
        }

    override fun <A> just(a: A): Kind<ForGen, A> =
        Gen { a }

    override fun <A, B> tailRecM(a: A, f: (A) -> Kind<ForGen, Either<A, B>>): Kind<ForGen, B> =
        Gen { rn ->
            f(a).fix().unGen(rn).fold({
                tailRecM(it, f).fix().unGen(rn)
            }, {
                it
            })
        }
}