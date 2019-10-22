package propCheck

import arrow.core.Eval
import arrow.core.Tuple2
import arrow.fx.IO
import arrow.syntax.function.andThen
import arrow.typeclasses.Show

/**
 * This file is really useful for having overloads without the need of reflection. Most ppl will only ever use
 *  Property or Boolean as their Testable
 */
fun mapResult(a: Boolean, f: (TestResult) -> TestResult): Property =
    mapResult(a.property(), f)
fun mapTotalResult(a: Boolean, f: (TestResult) -> TestResult): Property =
    mapTotalResult(a.property(), f)
fun mapRoseResult(a: Boolean, f: (Rose<TestResult>) -> Rose<TestResult>): Property =
    mapRoseResult(a.property(), f)
fun mapProp(a: Boolean, f: (Prop) -> Prop): Property = mapProp(a.property(), f)
fun mapSize(a: Boolean, f: (Int) -> Int): Property = mapSize(a.property(), f)
fun <B>shrinking(shrink: (B) -> Sequence<B>, arg: B, pf: (B) -> Boolean): Property =
    shrinking(shrink, arg, pf.andThen { b: Boolean -> b.property() })
fun noShrinking(a: Boolean): Property = noShrinking(a.property())
fun counterexample(s: () -> String, a: Boolean): Property = counterexample(s, a.property())
fun expectFailure(a: Boolean): Property = expectFailure(a.property())
fun once(a: Boolean): Property = once(a.property())
fun again(a: Boolean): Property = again(a.property())
fun withMaxSuccess(maxSuccess: Int, a: Boolean): Property = withMaxSuccess(maxSuccess, a.property())
fun checkCoverage(a: Boolean, confidence: Confidence = Confidence()): Property = checkCoverage(a.property(), confidence)
fun label(s: String, a: Boolean): Property = label(s, a.property())
fun <B>collect(b: B, a: Boolean, showB: Show<B> = Show.any()): Property = collect(b, a.property(), showB)
fun classify(bool: Boolean, label: String, a: Boolean): Property = classify(bool, label, a.property())
fun cover(p: Double, bool: Boolean, label: String, a: Boolean): Property = cover(p, bool, label, a.property())
fun tabulate(key: String, labels: List<String>, a: Boolean): Property = tabulate(key, labels, a.property())
fun coverTable(key: String, values: List<Tuple2<String, Double>>, a: Boolean): Property =
    coverTable(key, values, a.property())
fun callback(cb: Callback, a: Boolean): Property = callback(cb, a.property())
fun whenFail(a: Boolean, f: () -> Unit): Property = whenFail(a.property(), f)
fun whenFailIO(a: Boolean, f: IO<Unit>): Property = whenFailIO(a.property(), f)
fun whenFailEvery(a: Boolean, f: () -> Unit): Property = whenFailEvery(a.property(), f)
fun whenFailEveryIO(a: Boolean, f: IO<Unit>): Property = whenFailEveryIO(a.property(), f)
fun verbose(a: Boolean): Property = verbose(a.property())
fun verboseShrinking(a: Boolean): Property = verboseShrinking(a.property())
fun ioProperty(a: IO<Boolean>): Property = ioProperty(a.map { it.property() })
fun idempotentIOProperty(a: IO<Boolean>): Property = idempotentIOProperty(a.map { it.property() })
fun choice(a: Boolean, b: Boolean): Property = choice(a.property(), b.property())
fun choice(a: Property, b: Boolean): Property = choice(a, b.property())
fun choice(a: Boolean, b: Property): Property = choice(a.property(), b)
fun and(a: Boolean, b: () -> Boolean): Property = and(a.property(), b.andThen { it.property() })
fun and(a: Property, b: () -> Boolean): Property = and(a, b.andThen { it.property() })
fun or(a: Boolean, b: () -> Boolean): Property = or(a.property(), b.andThen { it.property() })
fun or(a: Property, b: () -> Boolean): Property = or(a, b.andThen { it.property() })
fun and(a: Boolean, b: Eval<Boolean>): Property = and(a.property(), b.map { it.property() })
fun and(a: Property, b: Eval<Boolean>): Property = and(a, b.map { it.property() })
fun or(a: Boolean, b: Eval<Boolean>): Property = or(a.property(), b.map { it.property() })
fun or(a: Property, b: Eval<Boolean>): Property = or(a, b.map { it.property() })