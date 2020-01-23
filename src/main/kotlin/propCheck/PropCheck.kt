package propCheck

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.either.applicative.applicative
import arrow.core.extensions.id.traverse.traverse
import arrow.core.extensions.list.functorFilter.filterMap
import arrow.core.extensions.sequence.foldable.foldRight
import arrow.fx.IO
import arrow.fx.extensions.fx
import arrow.fx.extensions.io.functor.unit
import arrow.fx.extensions.io.monad.monad
import arrow.fx.fix
import arrow.mtl.OptionTPartialOf
import arrow.mtl.value
import arrow.recursion.elgotM
import arrow.typeclasses.Functor
import arrow.typeclasses.Monad
import kparsec.renderPretty
import kparsec.runParser
import kparsec.stream
import pretty.*
import propCheck.arbitrary.*
import propCheck.pretty.KValue
import propCheck.pretty.listParser
import propCheck.property.*
import propCheck.property.Failure
import kotlin.random.Random

/**
 * TODO Unsigned type instances for coarbitrary and function. Also generators for unsigned types
 * TODO Gen's and instances for arrow types
 * TODO's
 * - shrinking
 *  - pretty print shrink-trees/gens
 * - toString() output pretty printer
 *  - move to new lib to allow independent use
 */
fun checkGroup(groupName: String, props: List<Tuple2<String, Property>>): IO<Boolean> =
    detectConfig().flatMap { checkGroup(it, groupName, props) }

fun checkGroup(config: Config, groupName: String, props: List<Tuple2<String, Property>>): IO<Boolean> = IO.fx {
    !effect { println("━━━ $groupName ━━━") }

    val summary =
        props.fold(IO { Summary.monoid().empty().copy(waiting = PropertyCount(props.size)) }) { acc, (n, prop) ->
            IO.fx {
                val currSummary = acc.bind()
                val res = checkReport(config, PropertyName(n).some(), prop).bind()
                Summary.monoid().run {
                    currSummary + empty().copy(waiting = PropertyCount(-1)) +
                            when (res.status) {
                                is Result.Failure -> empty().copy(failed = PropertyCount(1))
                                is Result.Success -> empty().copy(successful = PropertyCount(1))
                                is Result.GivenUp -> empty().copy(gaveUp = PropertyCount(1))
                            }
                }
            }
        }.bind()

    summary.failed.unPropertyCount == 0 && summary.gaveUp.unPropertyCount == 0
}

fun check(propertyConfig: PropertyConfig = PropertyConfig(), c: suspend PropertyTestSyntax.() -> Unit): IO<Boolean> =
    check(property(propertyConfig, c))

fun check(
    config: Config,
    propertyConfig: PropertyConfig = PropertyConfig(),
    c: suspend PropertyTestSyntax.() -> Unit
): IO<Boolean> =
    check(config, property(propertyConfig, c))

fun check(prop: Property): IO<Boolean> =
    detectConfig().flatMap { check(it, prop) }

fun check(config: Config, prop: Property): IO<Boolean> = check(config, None, prop)

fun recheck(size: Size, seed: RandSeed, prop: Property): IO<Unit> =
    detectConfig().flatMap { recheck(it, size, seed, prop) }

fun recheck(
    size: Size,
    seed: RandSeed,
    propertyConfig: PropertyConfig = PropertyConfig(),
    c: suspend PropertyTestSyntax.() -> Unit
): IO<Unit> =
    recheck(size, seed, property(propertyConfig, c))

fun recheck(
    config: Config,
    size: Size,
    seed: RandSeed,
    propertyConfig: PropertyConfig = PropertyConfig(),
    c: suspend PropertyTestSyntax.() -> Unit
): IO<Unit> =
    recheck(config, size, seed, property(propertyConfig, c))

fun recheck(config: Config, size: Size, seed: RandSeed, prop: Property): IO<Unit> =
    checkReport(seed, size, config, None, prop).unit()

fun checkNamed(
    name: String,
    propertyConfig: PropertyConfig = PropertyConfig(),
    c: suspend PropertyTestSyntax.() -> Unit
): IO<Boolean> =
    checkNamed(name, property(propertyConfig, c))

fun checkNamed(name: String, prop: Property): IO<Boolean> =
    detectConfig().flatMap { check(it, PropertyName(name).some(), prop) }

fun checkNamed(
    config: Config,
    name: String,
    propertyConfig: PropertyConfig = PropertyConfig(),
    c: suspend PropertyTestSyntax.() -> Unit
): IO<Boolean> =
    check(config, PropertyName(name).some(), property(propertyConfig, c))

fun checkNamed(config: Config, name: String, prop: Property): IO<Boolean> =
    check(config, PropertyName(name).some(), prop)


fun check(config: Config, name: Option<PropertyName>, prop: Property): IO<Boolean> =
    checkReport(config, name, prop).map { it.status is Result.Success }

fun checkReport(name: Option<PropertyName>, prop: Property): IO<Report<Result>> =
    detectConfig().flatMap { c ->
        IO { RandSeed(Random.nextLong()) }.flatMap {
            checkReport(it, Size(0), c, name, prop)
        }
    }

fun checkReport(config: Config, name: Option<PropertyName>, prop: Property): IO<Report<Result>> =
    IO { RandSeed(Random.nextLong()) }.flatMap {
        checkReport(it, Size(0), config, name, prop)
    }

internal fun checkReport(
    seed: RandSeed,
    size: Size,
    config: Config,
    name: Option<PropertyName>,
    prop: Property
): IO<Report<Result>> =
    IO.fx {
        val report = !runProperty(IO.monad(), size, seed, prop.config, prop.prop) {
            // TODO Live update will come back once I finish concurrent output
            IO.unit
        }
        !IO { println(report.renderResult(config.useColor, name)) }
        report
    }.fix()

// ---------------- Running a single property
data class State(
    val numTests: TestCount,
    val numDiscards: DiscardCount,
    val size: Size,
    val seed: RandSeed,
    val coverage: Coverage<CoverCount>
)

// TODO also clean this up... split it apart etc
fun <M> runProperty(
    MM: Monad<M>,
    initialSize: Size,
    initialSeed: RandSeed,
    config: PropertyConfig,
    prop: PropertyT<M, Unit>,
    hook: (Report<Progress>) -> Kind<M, Unit>
): Kind<M, Report<Result>> {
    val (confidence, minTests) = when (config.terminationCriteria) {
        is EarlyTermination -> config.terminationCriteria.confidence.some() to TestLimit(config.terminationCriteria.limit)
        is NoEarlyTermination -> config.terminationCriteria.confidence.some() to TestLimit(config.terminationCriteria.limit)
        is NoConfidenceTermination -> None to TestLimit(config.terminationCriteria.limit)
    }

    fun successVerified(testCount: TestCount, coverage: Coverage<CoverCount>): Boolean =
        testCount.unTestCount.rem(100) == 0 && confidence.fold({ false }, { it.success(testCount, coverage) })

    fun failureVerified(testCount: TestCount, coverage: Coverage<CoverCount>): Boolean =
        testCount.unTestCount.rem(100) == 0 && confidence.fold({ false }, { it.failure(testCount, coverage) })

    return State(
        TestCount(0),
        DiscardCount(0),
        initialSize,
        initialSeed,
        Coverage.monoid(CoverCount.semigroup()).empty()
    ).elgotM({ MM.just(it.value()) }, { (numTests, numDiscards, size, seed, currCoverage) ->
        MM.run {
            hook(Report(numTests, numDiscards, currCoverage, Progress.Running)).flatMap {
                val coverageReached = successVerified(numTests, currCoverage)
                val coverageUnreachable = failureVerified(numTests, currCoverage)

                val enoughTestsRun = when (config.terminationCriteria) {
                    is EarlyTermination ->
                        numTests.unTestCount >= defaultMinTests.unTestLimit &&
                                (coverageReached || coverageUnreachable)
                    is NoEarlyTermination ->
                        numTests.unTestCount >= minTests.unTestLimit
                    is NoConfidenceTermination ->
                        numTests.unTestCount >= minTests.unTestLimit
                }

                when {
                    size.unSize > 99 ->
                        MM.just(Id(State(numTests, numDiscards, Size(0), seed, currCoverage)).right())
                    enoughTestsRun -> {
                        fun failureRep(msg: Doc<Markup>): Report<Result> = Report(
                            numTests, numDiscards, currCoverage,
                            Result.Failure(
                                FailureSummary(
                                    size, seed, ShrinkCount(0), msg, emptyList(), emptyList()
                                )
                            )
                        )

                        val successRep = Report(numTests, numDiscards, currCoverage, Result.Success)
                        val labelsCovered = currCoverage.coverageSuccess(numTests)
                        val confidenceReport =
                            if (coverageReached && labelsCovered) successRep
                            else failureRep("Test coverage cannot be reached after".text() spaced numTests.testCount())

                        val finalRep = when (config.terminationCriteria) {
                            is EarlyTermination -> confidenceReport
                            is NoEarlyTermination -> confidenceReport
                            is NoConfidenceTermination ->
                                if (labelsCovered) successRep
                                else failureRep("Labels not sufficiently covered after".text() spaced numTests.testCount())
                        }
                        MM.just(finalRep.left())
                    }
                    numDiscards.unDiscardCount >= config.maxDiscardRatio.unDiscardRatio * numTests.unTestCount.coerceAtLeast(
                        minTests.unTestLimit
                    ) ->
                        MM.just(Report(numTests, numDiscards, currCoverage, Result.GivenUp).left())
                    else -> seed.split().let { (s1, s2) ->
                        // TODO catch errors
                        MM.fx.monad {
                            val res = !prop.unPropertyT.runTestT
                                .value() // EitherT
                                .value().fix() // WriterT
                                .runGen(s1 toT size)
                                .runRose
                                .value() // OptionT

                            res.fold({
                                // discard
                                Id(
                                    State(
                                        numTests,
                                        DiscardCount(numDiscards.unDiscardCount + 1),
                                        Size(size.unSize + 1),
                                        s2,
                                        currCoverage
                                    )
                                ).right()
                            }, { node ->
                                node.res.let { (log, result) ->
                                    result.fold({
                                        // shrink failure
                                        val shrinkRes = !shrinkResult(
                                            MM,
                                            size,
                                            seed,
                                            config.shrinkLimit.unShrinkLimit,
                                            config.shrinkRetries.unShrinkRetries,
                                            node.some().unwrap(MM)
                                        ) { s ->
                                            hook(
                                                Report(
                                                    TestCount(numTests.unTestCount + 1),
                                                    numDiscards,
                                                    currCoverage,
                                                    Progress.Shrinking(s)
                                                )
                                            )
                                        }

                                        Report(
                                            TestCount(numTests.unTestCount + 1),
                                            numDiscards, currCoverage, shrinkRes
                                        ).left()
                                    }, {
                                        // test success
                                        val newCover = Coverage.monoid(CoverCount.semigroup()).run {
                                            log.coverage() + currCoverage
                                        }
                                        Id(
                                            State(
                                                TestCount(numTests.unTestCount + 1),
                                                numDiscards,
                                                Size(size.unSize + 1),
                                                s2,
                                                newCover
                                            )
                                        ).right()
                                    })
                                }
                            })
                        }
                    }
                }
            }
        }
    }, Id.traverse(), MM)
}

data class ShrinkState<M>(
    val numShrinks: ShrinkCount,
    val node: RoseF<Option<Tuple2<Log, Either<Failure, Unit>>>, Rose<M, Option<Tuple2<Log, Either<Failure, Unit>>>>>
)

// TODO inline classes for params
fun <M> shrinkResult(
    MM: Monad<M>,
    size: Size,
    seed: RandSeed,
    shrinkLimit: Int,
    shrinkRetries: Int,
    node: RoseF<Option<Tuple2<Log, Either<Failure, Unit>>>, Rose<M, Option<Tuple2<Log, Either<Failure, Unit>>>>>,
    hook: (FailureSummary) -> Kind<M, Unit>
): Kind<M, Result> =
    ShrinkState(ShrinkCount(0), node).elgotM({ MM.just(it.value()) }, { (numShrinks, curr) ->
        MM.fx.monad {
            curr.res.fold({
                Result.GivenUp.left()
            }, { (log, result) ->
                result.fold({
                    val summary = FailureSummary(
                        size, seed, numShrinks, it.unFailure,
                        annotations = log.unLog.filterMap {
                            if (it is JournalEntry.Annotate) FailureAnnotation.Annotation(it.text).some()
                            else if (it is JournalEntry.Input) FailureAnnotation.Input(it.text).some()
                            else None
                        },
                        footnotes = log.unLog.filterMap {
                            if (it is JournalEntry.Footnote) it.text.some()
                            else None
                        }
                    )

                    hook(summary).bind()

                    if (numShrinks.unShrinkCount >= shrinkLimit)
                        Result.Failure(summary).left()
                    else curr.shrunk.foldRight<Rose<M, Option<Tuple2<Log, Either<Failure, Unit>>>>, Kind<M, Either<Result, Id<ShrinkState<M>>>>>(
                        Eval.now(MM.just(Result.Failure(summary).left()))
                    ) { v, acc ->
                        Eval.now(
                            MM.fx.monad {
                                val r = v.runTreeN(MM, shrinkRetries).bind()
                                if (r.isFailure())
                                    Id(ShrinkState(ShrinkCount(numShrinks.unShrinkCount + 1), r)).right()
                                else acc.value().bind()
                            }
                        )
                    }.value().bind()
                }, { Result.Success.left() })
            })
        }
    }, Id.traverse(), MM)

fun <M, A, L, W> Rose<M, Option<Tuple2<W, Either<L, A>>>>.runTreeN(
    MM: Monad<M>,
    retries: Int
): Kind<M, RoseF<Option<Tuple2<W, Either<L, A>>>, Rose<M, Option<Tuple2<W, Either<L, A>>>>>> =
    MM.fx.monad {
        val r = runRose.bind()
        if (retries > 0 && r.isFailure().not())
            runTreeN(MM, retries - 1).bind()
        else r
    }

fun <M, A> Option<RoseF<A, Rose<OptionTPartialOf<M>, A>>>.unwrap(FF: Functor<M>): RoseF<Option<A>, Rose<M, Option<A>>> =
    fold({
        RoseF(None, emptySequence())
    }, { RoseF(it.res.some(), it.shrunk.map { FF.run { Rose(it.runRose.value().map { it.unwrap(FF) }) } }) })

fun <M, A, L, W> RoseF<Option<Tuple2<W, Either<L, A>>>, M>.isFailure(): Boolean =
    res.fold({ false }, { it.b.isLeft() })
