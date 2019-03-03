package io.jannis.propTest

import io.jannis.propTest.assertions.*
import io.kotlintest.properties.generateInfiniteSequence
import io.kotlintest.properties.shrinking.Shrinker

// unsafe helpers for those who are not using arrow
fun <A> Gen<A>.sample(): List<A> = sample().unsafeRunSync()

fun <A> Gen<A>.classify(n: Int, f: (A) -> Boolean, text: String): Unit =
    classify(n, text, f).unsafeRunSync()

fun <A> Gen<A>.tabulate(n: Int, text: String, f: (A) -> String): Unit =
    tabulate(n, text, f).unsafeRunSync()

fun propCheck(args: Args = Args(), f: () -> Property): Unit =
    propCheckIOWithError(args, f).unsafeRunSync()

fun propCheckWithResult(args: Args = Args(), f: () -> Property): Result =
    propCheckIO(args, f).unsafeRunSync()

fun <A> Arbitrary<A>.toKotlinTestGen(): io.kotlintest.properties.Gen<A> = object : io.kotlintest.properties.Gen<A> {
    override fun constants(): Iterable<A> = emptyList()

    override fun random(): Sequence<A> = generateInfiniteSequence {
        arbitrary().generate().unsafeRunSync()
    }

    override fun shrinker(): Shrinker<A>? = object : Shrinker<A> {
        override fun shrink(failure: A): List<A> = this@toKotlinTestGen.shrink(failure)
            .take(100).toList()
    }
}