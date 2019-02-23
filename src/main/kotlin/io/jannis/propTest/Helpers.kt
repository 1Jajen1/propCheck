package io.jannis.propTest

import io.jannis.propTest.instances.arbitrary
import arrow.typeclasses.Show
import io.kotlintest.properties.generateInfiniteSequence
import io.kotlintest.properties.shrinking.Shrinker

// unsafe helpers for those who are not using arrow
fun <A> Gen<A>.sample(): List<A> = sample().unsafeRunSync()

fun <A> Gen<A>.label(n: Int, f: (A) -> String): Unit =
    label(n, f).unsafeRunSync()

fun <A> Gen<A>.collect(n: Int, showA: Show<A> = Show.fromToString()): Unit =
    collect(n, showA).unsafeRunSync()

fun <A> Gen<A>.classify(n: Int, f: (A) -> Boolean, text: String): Unit =
    classify(n, text, f).unsafeRunSync()

fun <A> Gen<A>.tabulate(n: Int, text: String, f: (A) -> String): Unit =
    tabulate(n, text, f).unsafeRunSync()

inline fun <reified A> defArbitrary(): Arbitrary<A> = when (A::class::qualifiedName.get()) {
    "kotlin.Int", "java.lang.Integer" -> Int.arbitrary()
    "kotlin.Long", "java.lang.Long" -> Long.arbitrary()
    "kotlin.String", "java.lang.String" -> String.arbitrary()
    "kotlin.Char", "java.lang.Char" -> Char.arbitrary()
    "kotlin.Float", "java.lang.Float" -> Float.arbitrary()
    "kotlin.Double", "java.lang.Double" -> Double.arbitrary()
    "kotlin.Boolean", "java.lang.Boolean" -> Boolean.arbitrary()
    else -> throw IllegalStateException("Could not find default arbitrary for ${A::class::qualifiedName.get()}")
} as Arbitrary<A>

fun <A> Arbitrary<A>.toKotlinTestGen(): io.kotlintest.properties.Gen<A> = object : io.kotlintest.properties.Gen<A> {
    override fun constants(): Iterable<A> = emptyList()

    override fun random(): Sequence<A> = generateInfiniteSequence {
        arbitrary().generate().unsafeRunSync()
    }
    override fun shrinker(): Shrinker<A>? = object : Shrinker<A> {
        override fun shrink(failure: A): List<A> = this@toKotlinTestGen.shrink(failure)
            .take(100).toList()
        // TODO: This take should not be necessary, probably write own matchers that work over Arbitrary directly instead of this wrapper
    }
}