package io.jannis.propTest.assertions

import arrow.core.Tuple2
import arrow.effects.IO
import arrow.typeclasses.Show
import io.jannis.propTest.Gen
import io.jannis.propTest.assertions.property.testable.testable

// bool wrappers for easier use
fun mapResultP(f: (TestResult) -> TestResult): (Property) -> Property = Property.testable().mapResult(f)
fun mapTotalResultP(f: (TestResult) -> TestResult): (Property) -> Property = Property.testable().mapTotalResult(f)
fun mapRoseResultP(f: (Rose<TestResult>) -> Rose<TestResult>): (Property) -> Property = Property.testable().mapRoseResult(f)
fun mapPropP(f: (Prop) -> Prop): (Property) -> Property = Property.testable().mapProp(f)
fun mapSizeP(f: (Int) -> Int): (Property) -> Property = Property.testable().mapSize(f)
fun <B>shrinkingP(shrinkerB: (B) -> Sequence<B>, arg: B, pf: (B) -> Property): Property = Property.testable().shrinking(shrinkerB, arg, pf)
fun noShrinkingP(): (Property) -> Property = Property.testable().noShrinking()
fun counterexampleP(s: String): (Property) -> Property = Property.testable().counterexample(s)
fun expectFailureP(): (Property) -> Property = Property.testable().expectFailure()
fun onceP(): (Property) -> Property = Property.testable().once()
fun againP(): (Property) -> Property = Property.testable().again()
fun withMaxSuccessP(maxSuccess: Int): (Property) -> Property = Property.testable().withMaxSuccess(maxSuccess)
fun checkCoverageP(c: Confidence): (Property) -> Property = Property.testable().checkCoverage(c)
fun labelP(label: String): (Property) -> Property = Property.testable().label(label)
fun <B>collectP(showB: Show<B>, b: B): (Property) -> Property = Property.testable().collect(showB, b)
fun classifyP(bool: Boolean, label: String): (Property) -> Property = Property.testable().classify(bool, label)
fun coverP(p: Double, bool: Boolean, table: String): (Property) -> Property = Property.testable().cover(p, bool, table)
fun tabulateP(key: String, values: List<String>): (Property) -> Property = Property.testable().tabulate(key, values)
fun coverTableP(key: String, values: List<Tuple2<String, Double>>): (Property) -> Property = Property.testable().coverTable(key, values)
fun callbackP(cb: Callback): (Property) -> Property = Property.testable().callback(cb)
fun whenFailP(f: () -> Unit): (Property) -> Property = Property.testable().whenFail(f)
fun whenFailIOP(f: IO<Unit>): (Property) -> Property = Property.testable().whenFailIO(f)
fun whenFailEveryP(f: () -> Unit): (Property) -> Property = Property.testable().whenFailEvery(f)
fun whenFailEveryIOP(f: IO<Unit>): (Property) -> Property = Property.testable().whenFailEveryIO(f)
fun verboseP(): (Property) -> Property = Property.testable().verbose()
fun verboseShrinkingP(): (Property) -> Property = Property.testable().verboseShrinking()
fun assertP(bool: Boolean): (Property) -> Property = Property.testable().assert(bool)
fun <B> forAllP(showB: Show<B>, genB: Gen<B>): ((B) -> Property) -> Property = Property.testable().forAll(showB, genB)
fun <B> forAllBlindP(genB: Gen<B>): ((B) -> Property) -> Property = Property.testable().forAllBlind(genB)
fun <B> forAllShrinkP(showB: Show<B>, genB: Gen<B>, shrinkerB: (B) -> Sequence<B>): ((B) -> Property) -> Property = Property.testable().forAllShrink(showB, genB, shrinkerB)
fun <B> forAllShrinkShowP(genB: Gen<B>, shrinkerB: (B) -> Sequence<B>, showerB: (B) -> String): ((B) -> Property) -> Property = Property.testable().forAllShrinkShow(genB, shrinkerB, showerB)
fun <B> forAllShrinkBlindP(genB: Gen<B>, shrinkerB: (B) -> Sequence<B>): ((B) -> Property) -> Property = Property.testable().forAllShrinkBlind(genB, shrinkerB)