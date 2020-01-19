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
import arrow.syntax.collections.tail
import arrow.typeclasses.Functor
import arrow.typeclasses.Monad
import kparsec.renderPretty
import kparsec.runParser
import kparsec.stream
import pretty.doc
import pretty.pretty
import pretty.renderPretty
import pretty.renderString
import propCheck.arbitrary.*
import propCheck.pretty.*
import propCheck.property.*
import propCheck.property.Failure
import kotlin.random.Random

/**
 * Grand plan:
 * 1. Finish state machine testing + Function instances
 * 2. Better interop with kotlin ranges
 * 3. Polish api with syntax sugar
 * 4.1 Gradle test runner
 * 4.2 Test framework integration (kotlintest, spek etc)
 * 5. Write tests for propcheck itself
 * 6. Write ank checked documentation
 * 6.1. Have docs switchable between syntax sugar/fx/flatMap
 * 7. Polish and prepare release notes
 *
 * Somewhere after 6. publish release candidate in arrow channels
 * After 7 release 1.0
 *
 * Problems that need to be solved:
 * - Autobinding syntax sugar
 * - Gradle test runner won't be too easy to get right I think
 *
 * New Grand Plan:
 * - state machine testing
 *  - rewrite using rec-scheme
 *  - pretty print results
 * - Functions
 *  - easier access to toFunction
 * - writing gens
 *  - bind overload for `ForId` gens inside `M` genT's.
 *  - shorthand to get into `MonadGen`
 * - writing properties
 *  - look into autobinding eqv/neqv or provide should like alternative syntax which autobinds
 *  - same for success/discard etc
 * - shrinking
 *  - pretty print shrink-trees/gens
 * - toString() output pretty printer
 *  - move to new lib to allow independent use
 * - helpers
 *  - kotlin receiver syntax magic to define tests
 * - write a bunch of tests and try to notice flaws
 *  - Write tests for propCheck itself
 *  - Do a 0.10 release and write tests for the prettyprinter, kparsec and the toString output one
 */
fun main() {
    checkNamed("List.reverse", property {
        val xs = !forAll { int(0..100).list(0..100) }

        // TODO this is a bug in the parser, it should parse 0ö0 as string 0ö0 instead!
        xs.map { it.toString() }.roundtrip({ "[" + it.joinToString("ö") + "]" }, {
            listParser().runParser("", it)
                .map { (it as KValue.KList); it.vals.map { it.doc().pretty() } }
                .mapLeft { it.renderPretty(String.stream()) }
        }, Either.applicative()).bind()
    }).unsafeRunSync()

    checkNamed("Option.map") {
        // TODO add special IdGen overload to forAll etc which offers the extra functions
        //  like filter, toFunction, etc
        val opt = !forAll { int(0..100).option() }
        val f = !forAllFn { int(0..100).toGenT().toFunction(Int.func(), Int.coarbitrary()).fromGenT() }

        opt.map(f).eqv(opt.fold({ None }, { f(it + 1).some() })).bind()
    }.unsafeRunSync()

    val test = property {
        val (b, bs) = !forAll {
            choice(
                map(
                    long(Range.constant(0, Long.MIN_VALUE, Long.MAX_VALUE)),
                    long(Range.constant(0, Long.MIN_VALUE, Long.MAX_VALUE))
                ) { (l, r) -> l.toBigDecimal() * r.toBigDecimal() },
                long(Range.constant(0, Long.MIN_VALUE, Long.MAX_VALUE)).map { it.toBigDecimal() }
            ).product(
                tupled(
                    long(Range.constant(0, Long.MIN_VALUE, Long.MAX_VALUE)),
                    long(Range.constant(0, Long.MIN_VALUE, Long.MAX_VALUE))
                )
            )
        }
        val (l1, h1) = bs
        val (l, h) = if (l1 > h1) h1 toT l1 else l1 toT h1

        // TODO this is a good test to check coverage on

        val r = b.clamp(l, h)
        if (r < l || r >= h) !failWith("out of bounds".doc())
    }

    check(test).unsafeRunSync()
}

/**
 * Running properties:
 * Entry points: execPar/execSeq both take a config
 *  - execPar has a max worker options which defaults to availableThreads
 *  - both take a config which specifies pretty-print settings like ribbon- and max-width, color
 *     other options are short-circuit on failure
 *  - both run tests according to the config and accumulate results
 *   - (ListK<Report>, Option<Report>) contains all necessary information, passed tests + optional failure
 *    - at runtime I might want to use Either<Report, Unit> to have short-circuiting
 *  - both call into lifecycle methods:
 *   - I need some global state that tracks current running tests and stores their state
 *   - Test can either be:
 *      Scheduled(id: Int) // show as not yet running
 *      Ignored(reason: String) // show greyed out as skipped
 *      Running(iteration: Int) // show as running and update from 0..testLimit + some smart way to show discards
 *      Shrinking(iteration: Int) // show as failed + shrinking with the shrunkAttempts without showing max, completion is non-det
 *      GaveUp, Failed, Passed (all +report) // show properly
 *    - These also mix well with gradle/intellij reporting
 *  - Neither method actually prints directly to some accumulated console report, they only update the console
 *   - Progress updates can be configured in the config (passing handle like IO methods)
 *    - This means you can easily disable/enable different output handles
 *  - Neither method throws on test failure, this behaviour can be added back by a combinator on exec*.withException()
 * Other entry points:
 *  - recheck runs a single property with a specified seed and size (maybe integrate it with execSeq, execPar)
 *
 * execSeq:
 * - traverse { p -> genSeed().flatTap { s -> p.checkProperty(s) }.lift() // lift into EitherT<WriterT> }
 *  - Property.checkProperty(seed: RandSeed, hooks: Hooks): IO<Report>
 *   - update lifecycle hook scheduled -> running(0)
 *   - setup start state and unfold using elgotM with IO
 *    - the fold just unpacks the Id into IO
 *    - unfold:
 *     - update lifecycle hook Running(numTests + 1)
 *     - stop if any stop condition was reached
 *      - maxTests: lifecycle hook Passed(report)
 *      - gaveUp: lifecycle hook GaveUp(report)
 *     - run a single test
 *      - failed:
 *       - update lifecycle hook Shrinking(0)
 *       - start shrinking
 *        - setup shrink state
 *        - elgotM
 *         - unfold:
 *          - update lifecycle hook Shrinking(numShrinks + 1)
 *          - run a single shrink it
 *       - update lifecycle hook Failed(report)
 *       - return shrunk result
 *      - success:
 *       - recur
 *
 * execPar:
 *  Same as traverse but singular test runs are run in parallel
 * exec* overfloads that take [(String, Prop)], [(String, [Prop])] and [(String, [(String, Prop])]
 *  - those denote named properties, grouped properties and named grouped properties.
 *  - There is also the guarantee that execPar will execute props in a group sequentially
 */

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
    check(seed, size, config, None, prop).unit()

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

internal fun check(config: Config, name: Option<PropertyName>, prop: Property): IO<Boolean> =
    IO { RandSeed(Random.nextLong()) }.flatMap {
        check(it, Size(0), config, name, prop)
    }

internal fun check(
    seed: RandSeed,
    size: Size,
    config: Config,
    name: Option<PropertyName>,
    prop: Property
): IO<Boolean> =
    IO.fx {
        val report = !runProperty(IO.monad(), size, seed, prop.config, prop.prop) {
            // TODO this needs to be in some terminal lib
            // TODO check if when run by a gradle plugin this actually works
            if (config.verbose is Verbose.Normal)
                clearConsole().followedBy(IO { print(it.renderProgress(UseColor.EnableColor, name)) })
            else IO.unit
        }
        // TODO better terminal support, this is quite meh
        !clearConsole().followedBy(IO { println(report.renderResult(UseColor.EnableColor, name)) })
        report.status is Result.Success
    }.fix()

// ---------------- Running a single property
data class State(
    val numTests: Int,
    val numDiscards: Int,
    val size: Size,
    val seed: RandSeed
)

// Change these methods to be polymorphic over m
// TODO also clean this up... split it apart etc
fun <M> runProperty(
    MM: Monad<M>,
    initialSize: Size,
    initialSeed: RandSeed,
    config: PropertyConfig,
    prop: PropertyT<M, Unit>,
    hook: (Report<Progress>) -> Kind<M, Unit>
): Kind<M, Report<Result>> =
    State(0, 0, initialSize, initialSeed).elgotM({ MM.just(it.value()) }, { (numTests, numDiscards, size, seed) ->
        MM.run {
            hook(Report(numTests, numDiscards, Progress.Running)).followedBy(
                when {
                    numTests >= config.testLimit.unTestLimit ->
                        MM.just(Report(numTests, numDiscards, Result.Success).left())
                    size.unSize > 99 ->
                        MM.just(Id(State(numTests, numDiscards, Size(0), seed)).right())
                    numDiscards >= config.maxDiscardRatio.unDiscardRatio * numTests.coerceAtLeast(config.testLimit.unTestLimit) ->
                        MM.just(Report(numTests, numDiscards, Result.GivenUp).left())
                    else -> seed.split().let { (s1, s2) ->
                        MM.fx.monad {
                            val res = !prop.unPropertyT.runTestT
                                .value() // EitherT
                                .value().fix() // WriterT
                                .runGen(s1 toT size)
                                .runRose
                                .value() // OptionT

                            res.fold({
                                // discard
                                Id(State(numTests, numDiscards + 1, Size(size.unSize + 1), s2)).right()
                            }, { node ->
                                node.res.let { (_, result) ->
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
                                                    numTests + 1,
                                                    numDiscards,
                                                    Progress.Shrinking(s)
                                                )
                                            )
                                        }

                                        Report(
                                            numTests + 1,
                                            numDiscards, shrinkRes
                                        ).left()
                                    }, {
                                        // test success
                                        Id(State(numTests + 1, numDiscards, Size(size.unSize + 1), s2)).right()
                                    })
                                }
                            })
                        }
                    }
                }
            )
        }
    }, Id.traverse(), MM)

data class ShrinkState<M>(
    val numShrinks: Int,
    val node: RoseF<Option<Tuple2<Log, Either<Failure, Unit>>>, Rose<M, Option<Tuple2<Log, Either<Failure, Unit>>>>>
)

fun <M> shrinkResult(
    MM: Monad<M>,
    size: Size,
    seed: RandSeed,
    shrinkLimit: Int,
    shrinkRetries: Int,
    node: RoseF<Option<Tuple2<Log, Either<Failure, Unit>>>, Rose<M, Option<Tuple2<Log, Either<Failure, Unit>>>>>,
    hook: (FailureSummary) -> Kind<M, Unit>
): Kind<M, Result> =
    ShrinkState(0, node).elgotM({ MM.just(it.value()) }, { (numShrinks, curr) ->
        MM.fx.monad {
            curr.res.fold({
                Result.GivenUp.left()
            }, { (log, result) ->
                result.fold({
                    val summary = FailureSummary(
                        size, seed, numShrinks, it.unFailure,
                        log.unLog.filterMap {
                            if (it is JournalEntry.Annotate) it.text.some()
                            else None
                        },
                        log.unLog.filterMap {
                            if (it is JournalEntry.Footnote) it.text.some()
                            else None
                        }
                    )

                    hook(summary).bind()

                    if (numShrinks >= shrinkLimit)
                        Result.Failure(summary).left()
                    else curr.shrunk.foldRight<Rose<M, Option<Tuple2<Log, Either<Failure, Unit>>>>, Kind<M, Either<Result, Id<ShrinkState<M>>>>>(Eval.now(MM.just(Result.Failure(summary).left()))) { v, acc ->
                        Eval.now(
                            MM.fx.monad {
                                val r = v.runTreeN(MM, shrinkRetries).bind()
                                if (r.isFailure())
                                    Id(ShrinkState(numShrinks + 1, r)).right()
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

fun <M, A> Option<RoseF<A, Rose<OptionTPartialOf<M>, A>>>.unwrap(FF: Functor<M>): RoseF<Option<A>, Rose<M, Option<A>>> = fold({
    RoseF(None, emptySequence())
}, { RoseF(it.res.some(), it.shrunk.map { FF.run { Rose(it.runRose.value().map { it.unwrap(FF) }) } }) })

fun <M, A, L, W> RoseF<Option<Tuple2<W, Either<L, A>>>, M>.isFailure(): Boolean =
    res.fold({ false }, { it.b.isLeft() })
