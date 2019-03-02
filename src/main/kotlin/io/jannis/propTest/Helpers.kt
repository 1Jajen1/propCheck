package io.jannis.propTest

import arrow.core.*
import arrow.data.ListK
import arrow.data.MapK
import arrow.data.Nel
import arrow.data.SetK
import io.jannis.propTest.asciistring.arbitrary.arbitrary
import io.jannis.propTest.assertions.*
import io.jannis.propTest.blind.arbitrary.arbitrary
import io.jannis.propTest.fixed.arbitrary.arbitrary
import io.jannis.propTest.instances.arbitrary
import io.jannis.propTest.instances.listk.arbitrary.arbitrary
import io.jannis.propTest.instances.mapk.arbitrary.arbitrary
import io.jannis.propTest.instances.setk.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple10.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple11.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple12.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple13.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple14.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple15.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple16.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple17.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple18.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple19.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple2.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple20.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple21.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple22.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple3.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple4.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple5.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple6.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple7.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple8.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple9.arbitrary.arbitrary
import io.jannis.propTest.shrink2.arbitrary.arbitrary
import io.jannis.propTest.smart.arbitrary.arbitrary
import io.jannis.propTest.unicodestring.arbitrary.arbitrary
import io.kotlintest.properties.generateInfiniteSequence
import io.kotlintest.properties.shrinking.Shrinker
import java.lang.reflect.ParameterizedType
import java.lang.reflect.Type
import java.lang.reflect.WildcardType

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