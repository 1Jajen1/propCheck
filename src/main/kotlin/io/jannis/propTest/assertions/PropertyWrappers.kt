package io.jannis.propTest.assertions

import arrow.core.Tuple2
import arrow.effects.IO
import arrow.typeclasses.Show
import io.jannis.propTest.Arbitrary
import io.jannis.propTest.Gen
import io.jannis.propTest.assertions.property.testable.testable
import io.jannis.propTest.defArbitrary
import io.jannis.propTest.defShow

/**
 * Property wrappers for all combinators
 * Avoid the boilerplate of Property.testable().func
 */
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
fun checkCoverageP(c: Confidence = Confidence()): (Property) -> Property = Property.testable().checkCoverage(c)
fun labelP(label: String): (Property) -> Property = Property.testable().label(label)
fun <B>collectP(b: B, showB: Show<B> = Show.any()): (Property) -> Property = Property.testable().collect(b, showB)
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
fun <B> forAllP(genB: Gen<B>, showB: Show<B> = Show.any()): ((B) -> Property) -> Property = Property.testable().forAll(genB, showB)
fun <B> forAllBlindP(genB: Gen<B>): ((B) -> Property) -> Property = Property.testable().forAllBlind(genB)
fun <B> forAllShrinkP(genB: Gen<B>, showB: Show<B> = Show.any(), shrinkerB: (B) -> Sequence<B>): ((B) -> Property) -> Property = Property.testable().forAllShrink(genB, showB, shrinkerB)
fun <B> forAllShrinkShowP(genB: Gen<B>, shrinkerB: (B) -> Sequence<B>, showerB: (B) -> String): ((B) -> Property) -> Property = Property.testable().forAllShrinkShow(genB, shrinkerB, showerB)
fun <B> forAllShrinkBlindP(genB: Gen<B>, shrinkerB: (B) -> Sequence<B>): ((B) -> Property) -> Property = Property.testable().forAllShrinkBlind(genB, shrinkerB)
fun ioPropertyP(): (IO<Property>) -> Property = Property.testable().ioProperty()
fun idempotentIOPropertyP(): (IO<Property>) -> Property = Property.testable().idempotentIOProperty()
inline fun <reified B: Any> forAllP(arbB: Arbitrary<B> = defArbitrary(), showB: Show<B> = defShow()): ((B) -> Property) -> Property = Property.testable().forAll(arbB, showB)
inline fun <reified B: Any> forAllShrinkP(arbB: Arbitrary<B> = defArbitrary(), showB: Show<B> = defShow()): ((B) -> Property) -> Property = Property.testable().forAllShrink(arbB, showB)
inline fun <reified B: Any> forAllBlindP(arbB: Arbitrary<B> = defArbitrary()): ((B) -> Property) -> Property = Property.testable().forAllBlind(arbB)
inline fun <reified B: Any> forAllShrinkBlindP(arbB: Arbitrary<B> = defArbitrary()): ((B) -> Property) -> Property = Property.testable().forAllShrinkBlind(arbB)