package propCheck.property

import arrow.core.Eval
import arrow.core.Tuple2
import arrow.fx.ForIO
import arrow.fx.IO
import arrow.syntax.function.andThen
import arrow.typeclasses.Show
import propCheck.Confidence

/**
 * This file is really useful for having overloads without the need of reflection. Most ppl will only ever use
 *  Property or Boolean as their Testable
 */
fun mapResult(a: Boolean, f: (TestResult) -> TestResult): Property =
    propCheck.property.mapResult(a.property(), f)
fun mapTotalResult(a: Boolean, f: (TestResult) -> TestResult): Property =
    propCheck.property.mapTotalResult(a.property(), f)
fun mapRoseResult(a: Boolean, f: (Rose<ForIO, TestResult>) -> Rose<ForIO, TestResult>): Property =
    propCheck.property.mapRoseResult(a.property(), f)
fun mapProp(a: Boolean, f: (Prop) -> Prop): Property =
    propCheck.property.mapProp(a.property(), f)
fun mapSize(a: Boolean, f: (Int) -> Int): Property = propCheck.property.mapSize(a.property(), f)
fun <B>shrinking(shrink: (B) -> Sequence<B>, arg: B, pf: (B) -> Boolean): Property =
    propCheck.property.shrinking(shrink, arg, pf.andThen { b: Boolean -> b.property() })
fun noShrinking(a: Boolean): Property = propCheck.property.noShrinking(a.property())
fun counterexample(s: () -> String, a: Boolean): Property =
    propCheck.property.counterexample(s, a.property())
fun expectFailure(a: Boolean): Property = propCheck.property.expectFailure(a.property())
fun once(a: Boolean): Property = propCheck.property.once(a.property())
fun again(a: Boolean): Property = propCheck.property.again(a.property())
fun withMaxSuccess(maxSuccess: Int, a: Boolean): Property =
    propCheck.property.withMaxSuccess(maxSuccess, a.property())
fun checkCoverage(a: Boolean, confidence: Confidence = Confidence()): Property =
    propCheck.property.checkCoverage(a.property(), confidence)
fun label(s: String, a: Boolean): Property = propCheck.property.label(s, a.property())
fun <B>collect(b: B, a: Boolean, showB: Show<B> = Show.any()): Property =
    propCheck.property.collect(b, a.property(), showB)
fun classify(bool: Boolean, label: String, a: Boolean): Property =
    propCheck.property.classify(bool, label, a.property())
fun cover(p: Double, bool: Boolean, label: String, a: Boolean): Property =
    propCheck.property.cover(p, bool, label, a.property())
fun tabulate(key: String, labels: List<String>, a: Boolean): Property =
    propCheck.property.tabulate(key, labels, a.property())
fun coverTable(key: String, values: List<Tuple2<String, Double>>, a: Boolean): Property =
    propCheck.property.coverTable(key, values, a.property())
fun callback(cb: Callback, a: Boolean): Property = propCheck.property.callback(cb, a.property())
fun whenFail(a: Boolean, f: () -> Unit): Property = propCheck.property.whenFail(a.property(), f)
fun whenFailIO(a: Boolean, f: IO<Unit>): Property = propCheck.property.whenFailIO(a.property(), f)
fun whenFailEvery(a: Boolean, f: () -> Unit): Property =
    propCheck.property.whenFailEvery(a.property(), f)
fun whenFailEveryIO(a: Boolean, f: IO<Unit>): Property =
    propCheck.property.whenFailEveryIO(a.property(), f)
fun verbose(a: Boolean): Property = propCheck.property.verbose(a.property())
fun verboseShrinking(a: Boolean): Property = propCheck.property.verboseShrinking(a.property())
fun ioProperty(a: IO<Boolean>): Property = propCheck.property.ioProperty(a.map { it.property() })
fun idempotentIOProperty(a: IO<Boolean>): Property =
    propCheck.property.idempotentIOProperty(a.map { it.property() })
fun choice(a: Boolean, b: Boolean): Property = propCheck.property.choice(a.property(), b.property())
fun choice(a: Property, b: Boolean): Property =
    propCheck.property.choice(a, b.property())
fun choice(a: Boolean, b: Property): Property =
    propCheck.property.choice(a.property(), b)
fun and(a: Boolean, b: () -> Boolean): Property =
    propCheck.property.and(a.property(), b.andThen { it.property() })
fun and(a: Property, b: () -> Boolean): Property =
    propCheck.property.and(a, b.andThen { it.property() })
fun or(a: Boolean, b: () -> Boolean): Property =
    propCheck.property.or(a.property(), b.andThen { it.property() })
fun or(a: Property, b: () -> Boolean): Property =
    propCheck.property.or(a, b.andThen { it.property() })
fun and(a: Boolean, b: Eval<Boolean>): Property =
    propCheck.property.and(a.property(), b.map { it.property() })
fun and(a: Property, b: Eval<Boolean>): Property =
    propCheck.property.and(a, b.map { it.property() })
fun or(a: Boolean, b: Eval<Boolean>): Property =
    propCheck.property.or(a.property(), b.map { it.property() })
fun or(a: Property, b: Eval<Boolean>): Property =
    propCheck.property.or(a, b.map { it.property() })