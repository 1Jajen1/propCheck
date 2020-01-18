package propCheck.property

import arrow.Kind
import arrow.core.*
import arrow.extension
import arrow.mtl.EitherT
import arrow.mtl.WriterT
import arrow.mtl.WriterTPartialOf
import arrow.mtl.extensions.writert.applicative.applicative
import arrow.mtl.extensions.writert.functor.functor
import arrow.mtl.extensions.writert.monad.monad
import arrow.mtl.value
import arrow.typeclasses.*
import pretty.*
import pretty.symbols.colon
import propCheck.arbitrary.GenT
import propCheck.arbitrary.GenTPartialOf
import propCheck.arbitrary.gent.applicative.applicative
import propCheck.arbitrary.gent.functor.functor
import propCheck.pretty.ValueDiffF
import propCheck.pretty.diff
import propCheck.pretty.toDoc
import propCheck.property.log.monoid.monoid

// ---------------------------- TestT
typealias Test<A> = TestT<ForId, A>

// @higherkind boilerplate
class ForTestT private constructor() {
    companion object
}
typealias TestTOf<M, A> = arrow.Kind<TestTPartialOf<M>, A>
typealias TestTPartialOf<M> = arrow.Kind<ForTestT, M>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <M, A> TestTOf<M, A>.fix(): TestT<M, A> =
    this as TestT<M, A>

data class TestT<M, A>(val runTestT: EitherT<WriterTPartialOf<M, Log>, Failure, A>) : TestTOf<M, A> {

    fun <B> map(MM: Monad<M>, f: (A) -> B): TestT<M, B> = TestT(runTestT.map(WriterT.monad(MM, Log.monoid()), f))

    fun <B> ap(MM: Monad<M>, ff: TestT<M, (A) -> B>): TestT<M, B> =
        TestT(runTestT.ap(WriterT.monad(MM, Log.monoid()), ff.runTestT))

    companion object {
        fun <M, A> just(MM: Monad<M>, a: A): TestT<M, A> =
            TestT(EitherT.just(WriterT.applicative(MM, Log.monoid()), a))
    }
}

@extension
interface TestTFunctor<M> : Functor<TestTPartialOf<M>> {
    fun MM(): Monad<M>

    override fun <A, B> Kind<TestTPartialOf<M>, A>.map(f: (A) -> B): Kind<TestTPartialOf<M>, B> =
        fix().map(MM(), f)
}

@extension
interface TestTApplicative<M> : Applicative<TestTPartialOf<M>> {
    fun MM(): Monad<M>

    override fun <A, B> Kind<TestTPartialOf<M>, A>.ap(ff: Kind<TestTPartialOf<M>, (A) -> B>): Kind<TestTPartialOf<M>, B> =
        fix().ap(MM(), ff.fix())

    override fun <A> just(a: A): Kind<TestTPartialOf<M>, A> = TestT.just(MM(), a)
}

@extension
interface TestTMonad<M> : Monad<TestTPartialOf<M>> {
    fun MM(): Monad<M>

    override fun <A> just(a: A): Kind<TestTPartialOf<M>, A> =
        TestT(EitherT.just(WriterT.applicative(MM(), Log.monoid()), a))

    override fun <A, B> Kind<TestTPartialOf<M>, A>.flatMap(f: (A) -> Kind<TestTPartialOf<M>, B>): Kind<TestTPartialOf<M>, B> =
        TestT(fix().runTestT.flatMap(WriterT.monad(MM(), Log.monoid()), f andThen { it.fix().runTestT }))

    override fun <A, B> tailRecM(a: A, f: (A) -> Kind<TestTPartialOf<M>, Either<A, B>>): Kind<TestTPartialOf<M>, B> =
        f(a).flatMap {
            it.fold({
                tailRecM(it, f)
            }, {
                just(it)
            })
        }
}

@extension
interface TestTMonadTest<M> : MonadTest<TestTPartialOf<M>>, TestTMonad<M> {
    override fun MM(): Monad<M>

    override fun <A> Test<A>.liftTest(): Kind<TestTPartialOf<M>, A> = hoist(MM())
}

fun <M, A> GenT<M, A>.lift(MM: Monad<M>): TestT<GenTPartialOf<M>, A> = TestT(
    EitherT.liftF<WriterTPartialOf<GenTPartialOf<M>, Log>, Failure, A>(
        WriterT.functor(GenT.functor(MM)),
        WriterT.liftF(this@lift, Log.monoid(), GenT.applicative(MM))
    )
)

// TODO refractor when https://github.com/arrow-kt/arrow/pull/1767 is merged
fun <M, A> Test<A>.hoist(MM: Monad<M>): TestT<M, A> = TestT(EitherT(WriterT(MM.just(runTestT.value().value().value()))))

fun <M, A> Kind<M, Tuple2<Log, Either<Failure, A>>>.toTestT(): TestT<M, A> = TestT(EitherT(WriterT(this)))

fun <A> Tuple2<Log, Either<Failure, A>>.toTest(): Test<A> = Id(this).toTestT()

interface MonadTest<M> : Monad<M> {

    fun <A> Test<A>.liftTest(): Kind<M, A>

    fun writeLog(l: JournalEntry): Kind<M, Unit> =
        (Log(ListK.just(l)) toT Unit.right()).toTest().liftTest()

    fun failWith(msg: String): Kind<M, Unit> = failWith(msg.doc())

    fun failWith(msg: Doc<Markup>): Kind<M, Unit> =
        (Log(ListK.empty()) toT Failure(msg).left())
            .toTest().liftTest()

    fun annotate(msg: () -> Doc<Markup>): Kind<M, Unit> =
        writeLog(JournalEntry.Annotate(msg))

    fun footnote(msg: () -> Doc<Markup>): Kind<M, Unit> =
        writeLog(JournalEntry.Footnote(msg))

    fun failException(e: Exception): Kind<M, Unit> =
        failWith(
            "Exception:".text() softLine (e.message?.doc() ?: nil())
        )

    fun failure(): Kind<M, Unit> = failWith(nil())

    fun succeeded(): Kind<M, Unit> = just(Unit)

    fun assert(b: Boolean): Kind<M, Unit> =
        if (b) succeeded()
        else failure()

    fun <A> diff(a: A, other: A, SA: Show<A> = Show.any(), cmp: (A, A) -> Boolean): Kind<M, Unit> =
        if (cmp(a, other)) succeeded()
        else failWith(SA.run {
            val diff = a.show().diff(other.show())

            when (diff.unDiff) {
                is ValueDiffF.Same -> "━━━ Failed (no differences) ━━━".text() +
                        hardLine() + diff.toDoc()
                else -> {
                    // Not sure if this overloading of Markup.Result.Failed is good, but for now it works
                    "━━━ Failed (".text() +
                            "- lhs".text().annotate(Markup.Result.Failed) +
                            " =/= ".text() +
                            "+ rhs".text().annotate(Markup.Result.Success) +
                            ") ━━━".text() +
                            hardLine() + diff.toDoc()
                }
            }
        })

    fun <A> A.eqv(other: A, EQA: Eq<A> = Eq.any(), SA: Show<A> = Show.any()): Kind<M, Unit> =
        diff(this, other, SA) { a, b -> EQA.run { a.eqv(b) } }

    fun <A> A.neqv(other: A, EQA: Eq<A> = Eq.any(), SA: Show<A> = Show.any()): Kind<M, Unit> =
        diff(this, other, SA) { a, b -> EQA.run { a.neqv(b) } }
}
