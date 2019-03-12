package propCheck

import propCheck.assertions.*

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