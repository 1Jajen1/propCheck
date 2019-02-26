package io.jannis.propTest.matchers

import arrow.core.toT
import arrow.syntax.collections.firstOption
import arrow.typeclasses.Eq
import arrow.typeclasses.Show
import io.jannis.propTest.*
import io.jannis.propTest.instances.arbitrary

inline fun <reified A : Any> forAll(
    arbA: Arbitrary<A> = defArbitrary(),
    config: Config = Config(),
    showA: Show<A> = Show.any(),
    eqA: Eq<A> = Eq.any(),
    noinline discardF: PropTest<A> = { false },
    noinline f: PropTest<A>
): Unit = iterate({ res ->
    when (res) {
        is Result.Success -> _runTest(arbA, config, showA, eqA, runSafe(discardF), runSafe(f)).unsafeRunSync().combine(res)
        is Result.Failure -> res
    }
}, Result.Success(0, 0) as Result).take(config.maxSuccess + 1).let {
    it.firstOption { it is Result.Failure }.fold({ it.last() }, { it })
}.let { printResult(config, it) }.unsafeRunSync()

sealed class BinaryTree<A> {
    data class Leaf<A>(val v: A) : BinaryTree<A>()
    data class Branch<A>(val left: BinaryTree<A>, val right: BinaryTree<A>) : BinaryTree<A>()

    companion object {
        fun <A> arbitrary(arbA: Arbitrary<A>): Arbitrary<BinaryTree<A>> = object : Arbitrary<BinaryTree<A>> {
            override fun arbitrary(): Gen<BinaryTree<A>> = Gen.sized { size ->
                if (size == 1) arbA.arbitrary().map { Leaf(it) as BinaryTree<A> }
                else Gen.frequency(
                    1 toT arbA.arbitrary().map { Leaf(it) as BinaryTree<A> },
                    3 toT this.let {
                        Gen.monad().fx {
                            Branch(
                                it.arbitrary().resize(size - 1).bind(),
                                it.arbitrary().resize(size - 1).bind()
                            ) as BinaryTree<A>
                        }.fix()
                    }
                )
            }

            override fun shrink(fail: BinaryTree<A>): Sequence<BinaryTree<A>> = when (fail) {
                is Leaf -> arbA.shrink(fail.v).map { Leaf(it) }
                is Branch -> (shrink(fail.left) toT shrink(fail.right)).let {
                    it.a + it.b + it.a.map { Branch(it, fail.right) } + it.b.map { Branch(fail.left, it) }
                }
            }
        }
    }
}

object B : Arbitrary<BinaryTree<Int>> {
    override fun arbitrary(): Gen<BinaryTree<Int>> = BinaryTree.arbitrary(Int.arbitrary()).arbitrary()
        .resize(30)

    override fun shrink(fail: BinaryTree<Int>): Sequence<BinaryTree<Int>> = BinaryTree.arbitrary(Int.arbitrary())
        .shrink(fail)
}

fun main() {
    forAll(B, Config(maxShrinks = 1000), discardF = {
        it is BinaryTree.Leaf
    }) {
        (it is BinaryTree.Branch && it.left is BinaryTree.Branch &&
                it.left.left is BinaryTree.Leaf && it.left.left.v > 0).not()
    }
}