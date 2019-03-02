package io.jannis.propTest.assertions

import arrow.core.Tuple2
import arrow.effects.IO
import arrow.typeclasses.Show
import io.jannis.propTest.Arbitrary
import io.jannis.propTest.Gen
import io.jannis.propTest.assertions.testresult.testable.testable
import io.jannis.propTest.defArbitrary

// bool wrappers for easier use
fun mapResultT(f: (TestResult) -> TestResult): (TestResult) -> Property = TestResult.testable().mapResult(f)
fun mapTotalResultT(f: (TestResult) -> TestResult): (TestResult) -> Property = TestResult.testable().mapTotalResult(f)
fun mapRoseResultT(f: (Rose<TestResult>) -> Rose<TestResult>): (TestResult) -> Property = TestResult.testable().mapRoseResult(f)
fun mapPropT(f: (Prop) -> Prop): (TestResult) -> Property = TestResult.testable().mapProp(f)
fun mapSizeT(f: (Int) -> Int): (TestResult) -> Property = TestResult.testable().mapSize(f)
fun <B>shrinkingT(shrinkerB: (B) -> Sequence<B>, arg: B, pf: (B) -> TestResult): Property = TestResult.testable().shrinking(shrinkerB, arg, pf)
fun noShrinkingT(): (TestResult) -> Property = TestResult.testable().noShrinking()
fun counterexampleT(s: String): (TestResult) -> Property = TestResult.testable().counterexample(s)
fun expectFailureT(): (TestResult) -> Property = TestResult.testable().expectFailure()
fun onceT(): (TestResult) -> Property = TestResult.testable().once()
fun againT(): (TestResult) -> Property = TestResult.testable().again()
fun withMaxSuccessT(maxSuccess: Int): (TestResult) -> Property = TestResult.testable().withMaxSuccess(maxSuccess)
fun checkCoverageT(c: Confidence = Confidence()): (TestResult) -> Property = TestResult.testable().checkCoverage(c)
fun labelT(label: String): (TestResult) -> Property = TestResult.testable().label(label)
fun <B>collectT(b: B, showB: Show<B> = Show.any()): (TestResult) -> Property = TestResult.testable().collect(b, showB)
fun classifyT(bool: Boolean, label: String): (TestResult) -> Property = TestResult.testable().classify(bool, label)
fun coverT(p: Double, bool: Boolean, table: String): (TestResult) -> Property = TestResult.testable().cover(p, bool, table)
fun tabulateT(key: String, values: List<String>): (TestResult) -> Property = TestResult.testable().tabulate(key, values)
fun coverTableT(key: String, values: List<Tuple2<String, Double>>): (TestResult) -> Property = TestResult.testable().coverTable(key, values)
fun callbackT(cb: Callback): (TestResult) -> Property = TestResult.testable().callback(cb)
fun whenFailT(f: () -> Unit): (TestResult) -> Property = TestResult.testable().whenFail(f)
fun whenFailIOT(f: IO<Unit>): (TestResult) -> Property = TestResult.testable().whenFailIO(f)
fun whenFailEveryT(f: () -> Unit): (TestResult) -> Property = TestResult.testable().whenFailEvery(f)
fun whenFailEveryIOT(f: IO<Unit>): (TestResult) -> Property = TestResult.testable().whenFailEveryIO(f)
fun verboseT(): (TestResult) -> Property = TestResult.testable().verbose()
fun verboseShrinkingT(): (TestResult) -> Property = TestResult.testable().verboseShrinking()
fun assertT(bool: Boolean): (TestResult) -> Property = TestResult.testable().assert(bool)
fun <B> forAllT(genB: Gen<B>, showB: Show<B> = Show.any()): ((B) -> TestResult) -> Property = TestResult.testable().forAll(genB, showB)
fun <B> forAllBlindT(genB: Gen<B>): ((B) -> TestResult) -> Property = TestResult.testable().forAllBlind(genB)
fun <B> forAllShrinkT(genB: Gen<B>, showB: Show<B> = Show.any(), shrinkerB: (B) -> Sequence<B>): ((B) -> TestResult) -> Property = TestResult.testable().forAllShrink(genB, showB, shrinkerB)
fun <B> forAllShrinkShowT(genB: Gen<B>, shrinkerB: (B) -> Sequence<B>, showerB: (B) -> String): ((B) -> TestResult) -> Property = TestResult.testable().forAllShrinkShow(genB, shrinkerB, showerB)
fun <B> forAllShrinkBlindT(genB: Gen<B>, shrinkerB: (B) -> Sequence<B>): ((B) -> TestResult) -> Property = TestResult.testable().forAllShrinkBlind(genB, shrinkerB)
fun ioPropertyT(): (IO<TestResult>) -> Property = TestResult.testable().ioProperty()
fun idempotentIOPropertyT(): (IO<TestResult>) -> Property = TestResult.testable().idempotentIOProperty()
inline fun <reified B: Any> forAllT(arbB: Arbitrary<B> = defArbitrary(), showB: Show<B> = Show.any()): ((B) -> TestResult) -> Property = TestResult.testable().forAll(arbB, showB)
inline fun <reified B: Any> forAllShrinkT(arbB: Arbitrary<B> = defArbitrary(), showB: Show<B> = Show.any()): ((B) -> TestResult) -> Property = TestResult.testable().forAllShrink(arbB, showB)
inline fun <reified B: Any> forAllBlindT(arbB: Arbitrary<B> = defArbitrary()): ((B) -> TestResult) -> Property = TestResult.testable().forAllBlind(arbB)
inline fun <reified B: Any> forAllShrinkBlindT(arbB: Arbitrary<B> = defArbitrary()): ((B) -> TestResult) -> Property = TestResult.testable().forAllShrinkBlind(arbB)