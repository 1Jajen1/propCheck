package io.jannis.propTest.assertions

import arrow.core.Tuple2
import arrow.effects.IO
import arrow.typeclasses.Show
import io.jannis.propTest.Arbitrary
import io.jannis.propTest.Gen
import io.jannis.propTest.defArbitrary

// bool wrappers for easier use
fun mapResult(f: (TestResult) -> TestResult): (Boolean) -> Property = Boolean.testable().mapResult(f)
fun mapTotalResult(f: (TestResult) -> TestResult): (Boolean) -> Property = Boolean.testable().mapTotalResult(f)
fun mapRoseResult(f: (Rose<TestResult>) -> Rose<TestResult>): (Boolean) -> Property = Boolean.testable().mapRoseResult(f)
fun mapProp(f: (Prop) -> Prop): (Boolean) -> Property = Boolean.testable().mapProp(f)
fun mapSize(f: (Int) -> Int): (Boolean) -> Property = Boolean.testable().mapSize(f)
fun <B>shrinking(shrinkerB: (B) -> Sequence<B>, arg: B, pf: (B) -> Boolean): Property = Boolean.testable().shrinking(shrinkerB, arg, pf)
fun noShrinking(): (Boolean) -> Property = Boolean.testable().noShrinking()
fun counterexample(s: String): (Boolean) -> Property = Boolean.testable().counterexample(s)
fun expectFailure(): (Boolean) -> Property = Boolean.testable().expectFailure()
fun once(): (Boolean) -> Property = Boolean.testable().once()
fun again(): (Boolean) -> Property = Boolean.testable().again()
fun withMaxSuccess(maxSuccess: Int): (Boolean) -> Property = Boolean.testable().withMaxSuccess(maxSuccess)
fun checkCoverage(c: Confidence = Confidence()): (Boolean) -> Property = Boolean.testable().checkCoverage(c)
fun label(label: String): (Boolean) -> Property = Boolean.testable().label(label)
fun <B>collect(b: B, showB: Show<B> = Show.any()): (Boolean) -> Property = Boolean.testable().collect(b, showB)
fun classify(bool: Boolean, label: String): (Boolean) -> Property = Boolean.testable().classify(bool, label)
fun cover(p: Double, bool: Boolean, table: String): (Boolean) -> Property = Boolean.testable().cover(p, bool, table)
fun tabulate(key: String, values: List<String>): (Boolean) -> Property = Boolean.testable().tabulate(key, values)
fun coverTable(key: String, values: List<Tuple2<String, Double>>): (Boolean) -> Property = Boolean.testable().coverTable(key, values)
fun callback(cb: Callback): (Boolean) -> Property = Boolean.testable().callback(cb)
fun whenFail(f: () -> Unit): (Boolean) -> Property = Boolean.testable().whenFail(f)
fun whenFailIO(f: IO<Unit>): (Boolean) -> Property = Boolean.testable().whenFailIO(f)
fun whenFailEvery(f: () -> Unit): (Boolean) -> Property = Boolean.testable().whenFailEvery(f)
fun whenFailEveryIO(f: IO<Unit>): (Boolean) -> Property = Boolean.testable().whenFailEveryIO(f)
fun verbose(): (Boolean) -> Property = Boolean.testable().verbose()
fun verboseShrinking(): (Boolean) -> Property = Boolean.testable().verboseShrinking()
fun assert(bool: Boolean): (Boolean) -> Property = Boolean.testable().assert(bool)
fun <B> forAll(genB: Gen<B>, showB: Show<B> = Show.any()): ((B) -> Boolean) -> Property = Boolean.testable().forAll(genB, showB)
fun <B> forAllBlind(genB: Gen<B>): ((B) -> Boolean) -> Property = Boolean.testable().forAllBlind(genB)
fun <B> forAllShrink(genB: Gen<B>, showB: Show<B> = Show.any(), shrinkerB: (B) -> Sequence<B>): ((B) -> Boolean) -> Property = Boolean.testable().forAllShrink(genB, showB, shrinkerB)
fun <B> forAllShrinkShow(genB: Gen<B>, shrinkerB: (B) -> Sequence<B>, showerB: (B) -> String): ((B) -> Boolean) -> Property = Boolean.testable().forAllShrinkShow(genB, shrinkerB, showerB)
fun <B> forAllShrinkBlind(genB: Gen<B>, shrinkerB: (B) -> Sequence<B>): ((B) -> Boolean) -> Property = Boolean.testable().forAllShrinkBlind(genB, shrinkerB)
fun ioProperty(): (IO<Boolean>) -> Property = Boolean.testable().ioProperty()
fun idempotentIOProperty(): (IO<Boolean>) -> Property = Boolean.testable().idempotentIOProperty()
inline fun <reified B: Any> forAll(arbB: Arbitrary<B> = defArbitrary(), showB: Show<B> = Show.any()): ((B) -> Boolean) -> Property = Boolean.testable().forAll(arbB, showB)
inline fun <reified B: Any> forAllShrink(arbB: Arbitrary<B> = defArbitrary(), showB: Show<B> = Show.any()): ((B) -> Boolean) -> Property = Boolean.testable().forAllShrink(arbB, showB)
inline fun <reified B: Any> forAllBlind(arbB: Arbitrary<B> = defArbitrary()): ((B) -> Boolean) -> Property = Boolean.testable().forAllBlind(arbB)
inline fun <reified B: Any> forAllShrinkBlind(arbB: Arbitrary<B> = defArbitrary()): ((B) -> Boolean) -> Property = Boolean.testable().forAllShrinkBlind(arbB)