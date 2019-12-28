package propCheck.pretty.parse

import arrow.Kind
import arrow.Kind4
import arrow.core.*
import arrow.core.extensions.list.foldable.foldLeft
import arrow.extension
import arrow.syntax.collections.tail
import arrow.typeclasses.Alternative
import arrow.typeclasses.Eq
import arrow.typeclasses.Foldable
import arrow.typeclasses.Monad

data class Hints<T>(val hints: List<Set<ErrorItem<T>>>) {
    operator fun plus(other: Hints<T>): Hints<T> = Hints(hints + other.hints)

    companion object {
        fun <T> empty(): Hints<T> = Hints(emptyList())
    }

    fun refreshHints(l: Option<ErrorItem<T>>): Hints<T> =
        if (hints.isEmpty()) this
        else l.fold({ Hints(hints.tail()) }, { Hints(listOf(setOf(it)) + hints.tail()) })
}

sealed class ErrorItem<T> {
    data class Tokens<T>(val ts: NonEmptyList<T>) : ErrorItem<T>()
    data class Label<T>(val ts: String) : ErrorItem<T>()
    class EndOfInput<T> : ErrorItem<T>()
}

sealed class ParserState<E, EL, A> {
    data class ConsumedOk<E, EL, A>(val a: A, val hints: Hints<EL>) : ParserState<E, EL, A>()
    data class ConsumedError<E, EL, A>(val e: ParsecError<E, EL>) : ParserState<E, EL, A>()
    data class EmptyOk<E, EL, A>(val a: A, val hints: Hints<EL>) : ParserState<E, EL, A>()
    data class EmptyError<E, EL, A>(val e: ParsecError<E, EL>) : ParserState<E, EL, A>()

    fun <B> map(f: (A) -> B): ParserState<E, EL, B> = when (this) {
        is ConsumedOk -> ConsumedOk(f(a), hints)
        is EmptyOk -> EmptyOk(f(a), hints)
        else -> this as ParserState<E, EL, B>
    }
}

class ForKParsecT private constructor()
typealias KParsecTOf<E, I, EL, M, A> = Kind<KParsecTPartialOf<E, I, EL, M>, A>
typealias KParsecTPartialOf<E, I, EL, M> = Kind4<ForKParsecT, E, I, EL, M>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <E, I, EL, M, A> KParsecTOf<E, I, EL, M, A>.fix(): KParsecT<E, I, EL, M, A> =
    this as KParsecT<E, I, EL, M, A>

data class KParsecT<E, I, EL, M, A>(
    val MM: Monad<M>,
    val pFun: (State<I>) -> Kind<M, Tuple2<ParserState<E, EL, A>, State<I>>>
) : KParsecTOf<E, I, EL, M, A> {

    fun <B> map(f: (A) -> B): KParsecT<E, I, EL, M, B> =
        KParsecT(MM, AndThen(pFun).andThen { MM.run { it.map { (s, i) -> s.map(f) toT i } } })

    fun <B> ap(ff: KParsecT<E, I, EL, M, (A) -> B>): KParsecT<E, I, EL, M, B> = lazyAp { ff }

    fun <B> lazyAp(ff: () -> KParsecT<E, I, EL, M, (A) -> B>): KParsecT<E, I, EL, M, B> =
        KParsecT(MM, AndThen(pFun).andThen {
            MM.run {
                it.flatMap { (a, i) ->
                    when (a) {
                        is ParserState.ConsumedOk -> ff().pFun(i).map { (ffa, s) ->
                            when (ffa) {
                                is ParserState.ConsumedOk -> ParserState.ConsumedOk(ffa.a(a.a), ffa.hints)
                                is ParserState.EmptyOk -> ParserState.ConsumedOk(ffa.a(a.a), a.hints + ffa.hints)
                                is ParserState.EmptyError -> ParserState.EmptyError(ffa.e.withHints(a.hints))
                                else -> (ffa as ParserState<E, EL, B>)
                            } toT s
                        }
                        is ParserState.EmptyOk -> ff().pFun(i).map { (ffa, s) ->
                            when (ffa) {
                                is ParserState.ConsumedOk -> ParserState.ConsumedOk(ffa.a(a.a), ffa.hints)
                                is ParserState.EmptyOk -> ParserState.EmptyOk(ffa.a(a.a), a.hints + ffa.hints)
                                is ParserState.EmptyError -> ParserState.EmptyError(ffa.e.withHints(a.hints))
                                else -> (ffa as ParserState<E, EL, B>)
                            } toT s
                        }
                        else -> MM.just((a as ParserState<E, EL, B>) toT i)
                    }
                }
            }
        })

    fun <B> flatMap(f: (A) -> KParsecT<E, I, EL, M, B>): KParsecT<E, I, EL, M, B> =
        KParsecT(MM, AndThen(pFun).andThen {
            MM.run {
                it.flatMap { (pa, s) ->
                    when (pa) {
                        is ParserState.ConsumedOk -> f(pa.a).pFun(s).map { (pa2, s2) ->
                            when (pa2) {
                                is ParserState.EmptyOk -> ParserState.EmptyOk(pa2.a, pa.hints + pa2.hints)
                                is ParserState.EmptyError -> ParserState.EmptyError(pa2.e.withHints(pa.hints))
                                else -> pa2
                            } toT s2
                        }
                        is ParserState.EmptyOk -> f(pa.a).pFun(s).map { (pa2, s2) ->
                            when (pa2) {
                                is ParserState.EmptyOk -> ParserState.EmptyOk(pa2.a, pa.hints + pa2.hints)
                                is ParserState.EmptyError -> ParserState.EmptyError<E, EL, B>(pa2.e.withHints(pa.hints))
                                else -> pa2
                            } toT s2
                        }
                        else -> MM.just((pa as ParserState<E, EL, B>) toT s)
                    }
                }
            }
        })

    fun orElse(other: KParsecT<E, I, EL, M, A>): KParsecT<E, I, EL, M, A> =
        KParsecT(MM, AndThen {
            MM.run {
                pFun(it).flatMap { (a, i) ->
                    when (a) {
                        is ParserState.ConsumedOk,
                        is ParserState.EmptyOk -> MM.just(a toT i)
                        is ParserState.EmptyError -> other.pFun(it).map { (pa, s) ->
                            when (pa) {
                                is ParserState.ConsumedError -> ParserState.ConsumedError<E, EL, A>(a.e + pa.e) toT i.longestMatch(
                                    s
                                )
                                is ParserState.EmptyError -> ParserState.EmptyError<E, EL, A>(a.e + pa.e) toT i.longestMatch(
                                    s
                                )
                                is ParserState.EmptyOk -> ParserState.EmptyOk<E, EL, A>(
                                    pa.a,
                                    a.e.toHints(s.offset) + pa.hints
                                ) toT s
                                is ParserState.ConsumedOk -> pa toT s
                            }
                        }
                        is ParserState.ConsumedError -> MM.just(a toT i)
                    }
                }
            }
        })

    fun runParsecT(s: State<I>): Kind<M, Either<ParsecError<E, EL>, A>> = MM.run {
        pFun(s).map { (pa, _) ->
            when (pa) {
                is ParserState.ConsumedOk -> pa.a.right()
                is ParserState.EmptyOk -> pa.a.right()
                is ParserState.ConsumedError -> pa.e.left()
                is ParserState.EmptyError -> pa.e.left()
            }
        }
    }

    companion object {
        fun <E, I, EL, CHUNK, M> monadParsec(
            SI: Stream<I, EL, CHUNK>,
            MM: Monad<M>
        ): MonadParsec<E, I, EL, CHUNK, KParsecTPartialOf<E, I, EL, M>> =
            object : KParsecTMonadParsec<E, I, EL, CHUNK, M> {
                override fun MM(): Monad<M> = MM
                override fun SI(): Stream<I, EL, CHUNK> = SI
            }
    }
}

@extension
interface KParsecTMonadParsec<E, I, EL, CHUNK, M> : MonadParsec<E, I, EL, CHUNK, KParsecTPartialOf<E, I, EL, M>> {
    override fun SI(): Stream<I, EL, CHUNK>
    fun MM(): Monad<M>

    override fun <A> empty(): Kind<KParsecTPartialOf<E, I, EL, M>, A> = KParsecT(MM(), AndThen {
        MM().just(ParserState.EmptyError<E, EL, A>(ParsecError.Trivial(it.offset, None, emptySet())) toT it)
    })

    override fun <A> Kind<KParsecTPartialOf<E, I, EL, M>, A>.some(): Kind<KParsecTPartialOf<E, I, EL, M>, SequenceK<A>> =
        fix().lazyAp { many().fix().map { xs -> { x: A -> (sequenceOf(x) + xs).k() } } }

    override fun <A> Kind<KParsecTPartialOf<E, I, EL, M>, A>.orElse(b: Kind<KParsecTPartialOf<E, I, EL, M>, A>): Kind<KParsecTPartialOf<E, I, EL, M>, A> =
        fix().orElse(b.fix())

    override fun <A, B> Kind<KParsecTPartialOf<E, I, EL, M>, A>.flatMap(f: (A) -> Kind<KParsecTPartialOf<E, I, EL, M>, B>): Kind<KParsecTPartialOf<E, I, EL, M>, B> =
        fix().flatMap(f.andThen { it.fix() })

    override fun <A> just(a: A): Kind<KParsecTPartialOf<E, I, EL, M>, A> = KParsecT(MM(), AndThen {
        MM().just(ParserState.EmptyOk<E, EL, A>(a, Hints.empty()) toT it)
    })

    override fun <A, B> tailRecM(
        a: A,
        f: (A) -> Kind<KParsecTPartialOf<E, I, EL, M>, Either<A, B>>
    ): Kind<KParsecTPartialOf<E, I, EL, M>, B> = KParsecT(MM(), AndThen {
        MM().run {
            f(a).fix().pFun(it).flatMap { (pa, s) ->
                when (pa) {
                    is ParserState.ConsumedOk -> pa.a.fold({ tailRecM(it, f).fix().pFun(s) }, {
                        just(ParserState.ConsumedOk<E, EL, B>(it, pa.hints) toT s)
                    })
                    is ParserState.EmptyOk -> pa.a.fold({ tailRecM(it, f).fix().pFun(s) }, {
                        just(ParserState.ConsumedOk<E, EL, B>(it, pa.hints) toT s)
                    })
                    else -> just((pa as ParserState<E, EL, B>) toT s)
                }
            }
        }
    })

    override fun <A> Kind<KParsecTPartialOf<E, I, EL, M>, A>.label(str: String): Kind<KParsecTPartialOf<E, I, EL, M>, A> =
        KParsecT(MM(), AndThen(fix().pFun).andThen {
            MM().run {
                it.map { (pa, s) ->
                    val l = if (str.isEmpty()) None
                    else ErrorItem.Label<EL>(str).some()

                    when (pa) {
                        is ParserState.ConsumedOk -> ParserState.ConsumedOk(
                            pa.a,
                            l.fold({ pa.hints }, { pa.hints.refreshHints(None) })
                        )
                        is ParserState.EmptyOk -> ParserState.EmptyOk(pa.a, pa.hints.refreshHints(l))
                        is ParserState.EmptyError -> ParserState.EmptyError<E, EL, A>(
                            when (val err = pa.e) {
                                is ParsecError.Trivial -> ParsecError.Trivial(
                                    err.offset,
                                    err.unexpectedTokens,
                                    l.fold({ emptySet<ErrorItem<EL>>() }, { setOf(it) })
                                )
                                else -> err
                            }
                        )
                        is ParserState.ConsumedError -> pa
                    } toT s
                }
            }
        })

    override fun eof(): Kind<KParsecTPartialOf<E, I, EL, M>, Unit> = KParsecT(MM(), AndThen { s ->
        SI().run {
            MM().just(
                s.input.takeOne().fold({
                    ParserState.EmptyOk<E, EL, Unit>(Unit, Hints.empty()) toT s
                }, { (x, _) ->
                    ParserState.EmptyError<E, EL, Unit>(
                        ParsecError.Trivial(
                            s.offset, ErrorItem.Tokens(Nel(x)).some(), setOf(ErrorItem.EndOfInput())
                        )
                    ) toT s
                })
            )
        }
    })

    override fun getParserState(): Kind<KParsecTPartialOf<E, I, EL, M>, State<I>> = KParsecT(MM(), AndThen {
        MM().just(
            ParserState.EmptyOk<E, EL, State<I>>(it, Hints.empty()) toT it
        )
    })

    override fun <A> Kind<KParsecTPartialOf<E, I, EL, M>, A>.lookAhead(): Kind<KParsecTPartialOf<E, I, EL, M>, A> =
        KParsecT(MM(), AndThen { s ->
            MM().run {
                fix().pFun(s).map { (pa, _) ->
                    when (pa) {
                        is ParserState.ConsumedOk -> ParserState.EmptyOk<E, EL, A>(pa.a, Hints.empty()) toT s
                        is ParserState.EmptyOk -> ParserState.EmptyOk<E, EL, A>(pa.a, Hints.empty()) toT s
                        else -> pa toT s
                    }
                }
            }
        })

    override fun <A> Kind<KParsecTPartialOf<E, I, EL, M>, A>.notFollowedBy(): Kind<KParsecTPartialOf<E, I, EL, M>, Unit> =
        KParsecT(MM(), AndThen { s ->
            MM().run {
                fix().pFun(s).map { (pa, _) ->
                    val what = {
                        SI().run {
                            s.input.takeOne().fold({ ErrorItem.EndOfInput<EL>() }, { ErrorItem.Tokens(Nel(it.a)) })
                        }
                    }
                    val tErr = { ParsecError.Trivial<E, EL>(s.offset, what().some(), emptySet()) }
                    when (pa) {
                        is ParserState.ConsumedOk -> ParserState.EmptyError<E, EL, Unit>(tErr()) toT s
                        is ParserState.EmptyOk -> ParserState.EmptyError<E, EL, Unit>(tErr()) toT s
                        is ParserState.ConsumedError -> ParserState.EmptyOk<E, EL, Unit>(Unit, Hints.empty()) toT s
                        is ParserState.EmptyError -> ParserState.EmptyOk<E, EL, Unit>(Unit, Hints.empty()) toT s
                    }
                }
            }
        })

    override fun <A> Kind<KParsecTPartialOf<E, I, EL, M>, A>.observing(): Kind<KParsecTPartialOf<E, I, EL, M>, Either<ParsecError<E, EL>, A>> =
        KParsecT(MM(), AndThen(fix().pFun).andThen {
            MM().run {
                it.map { (pa, s) ->
                    when (pa) {
                        is ParserState.ConsumedOk -> ParserState.ConsumedOk<E, EL, Either<ParsecError<E, EL>, A>>(
                            pa.a.right(),
                            Hints.empty()
                        )
                        is ParserState.ConsumedError -> ParserState.ConsumedOk<E, EL, Either<ParsecError<E, EL>, A>>(
                            pa.e.left(),
                            Hints.empty()
                        )
                        is ParserState.EmptyOk -> ParserState.EmptyOk<E, EL, Either<ParsecError<E, EL>, A>>(
                            pa.a.right(),
                            Hints.empty()
                        )
                        is ParserState.EmptyError -> ParserState.EmptyOk<E, EL, Either<ParsecError<E, EL>, A>>(
                            pa.e.left(),
                            pa.e.toHints(s.offset)
                        )
                    } toT s
                }
            }
        })

    override fun take(label: Option<String>, n: Int): Kind<KParsecTPartialOf<E, I, EL, M>, CHUNK> =
        KParsecT(MM(), AndThen {
            SI().run {
                it.input.take(n).fold({
                    MM().just(ParserState.EmptyError<E, EL, CHUNK>(ParsecError.Trivial(
                        it.offset,
                        ErrorItem.EndOfInput<EL>().some(),
                        label.filter { it.isNotEmpty() }.fold({ emptySet<ErrorItem<EL>>() }, { label ->
                            setOf(ErrorItem.Label(label))
                        })
                    )
                    ) toT it
                    )
                }, { (c, i) ->
                    if (c.size() != n) MM().just(ParserState.EmptyError<E, EL, CHUNK>(ParsecError.Trivial(
                        it.offset + c.size(),
                        ErrorItem.EndOfInput<EL>().some(),
                        label.filter { it.isNotEmpty() }.fold({ emptySet<ErrorItem<EL>>() }, { label ->
                            setOf(ErrorItem.Label(label))
                        })
                    )
                    ) toT it
                    )
                    else MM().just(ParserState.ConsumedOk<E, EL, CHUNK>(c, Hints.empty()) toT State(i, it.offset + n))
                })
            }
        })

    override fun takeAtLeastOneWhile(
        label: Option<String>,
        matcher: (EL) -> Boolean
    ): Kind<KParsecTPartialOf<E, I, EL, M>, CHUNK> = KParsecT(MM(), AndThen {
        SI().run {
            it.input.takeWhile(matcher).let { (chunk, rem) ->
                if (chunk.isEmpty()) MM().just(ParserState.EmptyError<E, EL, CHUNK>(ParsecError.Trivial<E, EL>(
                    it.offset,
                    it.input.takeOne().fold({ ErrorItem.EndOfInput<EL>() },
                        { (x, _) -> ErrorItem.Tokens(Nel(x)) }).some(),
                    label.filter { it.isNotEmpty() }.fold(
                        { emptySet<ErrorItem<EL>>() },
                        { l -> setOf(ErrorItem.Label(l)) })
                )
                ) toT it
                )
                else MM().just(ParserState.ConsumedOk<E, EL, CHUNK>(chunk, Hints(
                    label.filter { it.isNotEmpty() }.fold(
                        { emptyList<Set<ErrorItem<EL>>>() },
                        { l -> listOf(setOf(ErrorItem.Label(l))) })
                )
                ) toT State(rem, it.offset + chunk.size())
                )
            }
        }
    })

    override fun takeWhile(
        label: Option<String>,
        matcher: (EL) -> Boolean
    ): Kind<KParsecTPartialOf<E, I, EL, M>, CHUNK> = KParsecT(MM(), AndThen {
        SI().run {
            it.input.takeWhile(matcher).let { (chunk, rem) ->
                if (chunk.isEmpty()) MM().just(
                    ParserState.EmptyOk<E, EL, CHUNK>(chunk, Hints.empty()) toT State(
                        rem,
                        it.offset + chunk.size()
                    )
                )
                else MM().just(
                    ParserState.ConsumedOk<E, EL, CHUNK>(chunk, Hints.empty()) toT State(
                        rem,
                        it.offset + chunk.size()
                    )
                )
            }
        }
    })

    override fun <A> token(
        expected: Set<ErrorItem<EL>>,
        matcher: (EL) -> Option<A>
    ): Kind<KParsecTPartialOf<E, I, EL, M>, A> = KParsecT(MM(), AndThen {
        SI().run {
            it.input.takeOne().fold({
                MM().just(
                    ParserState.EmptyError<E, EL, A>(
                        ParsecError.Trivial(
                            it.offset,
                            ErrorItem.EndOfInput<EL>().some(),
                            expected
                        )
                    ) toT it
                )
            }, { (el, rem) ->
                matcher(el).fold({
                    MM().just(
                        ParserState.EmptyError<E, EL, A>(
                            ParsecError.Trivial(
                                it.offset,
                                ErrorItem.Tokens(Nel(el)).some(),
                                expected
                            )
                        ) toT it
                    )
                }, { a ->
                    MM().just(ParserState.ConsumedOk<E, EL, A>(a, Hints.empty()) toT State(rem, it.offset + 1))
                })
            })
        }
    })

    override fun tokens(chunk: CHUNK): Kind<KParsecTPartialOf<E, I, EL, M>, CHUNK> = KParsecT(MM(), AndThen {
        SI().run {
            it.input.take(chunk.size()).fold({
                MM().just(
                    ParserState.EmptyError<E, EL, CHUNK>(
                        ParsecError.Trivial(
                            it.offset,
                            ErrorItem.EndOfInput<EL>().some(),
                            setOf(ErrorItem.Tokens(Nel.fromListUnsafe(chunk.toTokens())))
                        )
                    ) toT it
                )
            }, { (c, rem) ->
                if (c.size() != chunk.size()) MM().just(
                    ParserState.EmptyError<E, EL, CHUNK>(
                        ParsecError.Trivial(
                            it.offset,
                            ErrorItem.Tokens(Nel.fromListUnsafe(c.toTokens())).some(),
                            setOf(ErrorItem.Tokens(Nel.fromListUnsafe(chunk.toTokens())))
                        )
                    ) toT it
                )
                else MM().just(
                    ParserState.ConsumedOk<E, EL, CHUNK>(c, Hints.empty()) toT State(
                        rem,
                        it.offset + c.size()
                    )
                )
            })
        }
    })

    override fun updateParserState(f: (State<I>) -> State<I>): Kind<KParsecTPartialOf<E, I, EL, M>, Unit> =
        KParsecT(MM(), AndThen { MM().just(ParserState.EmptyOk<E, EL, Unit>(Unit, Hints.empty()) toT f(it)) })

    override fun <A> Kind<KParsecTPartialOf<E, I, EL, M>, A>.tryP(): Kind<KParsecTPartialOf<E, I, EL, M>, A> =
        KParsecT(MM(), AndThen {
            MM().run {
                fix().pFun(it).map { (pa, s) ->
                    when (pa) {
                        is ParserState.EmptyError -> ParserState.EmptyError<E, EL, A>(pa.e) toT it
                        else -> pa toT s
                    }
                }
            }
        })

    override fun <A> Kind<KParsecTPartialOf<E, I, EL, M>, A>.withRecovery(h: (ParsecError<E, EL>) -> Kind<KParsecTPartialOf<E, I, EL, M>, A>): Kind<KParsecTPartialOf<E, I, EL, M>, A> =
        KParsecT(MM(), AndThen {
            MM().run {
                fix().pFun(it).flatMap { (pa, i) ->
                    when (pa) {
                        is ParserState.ConsumedError -> h(pa.e).fix().pFun(i)
                        is ParserState.EmptyError -> h(pa.e).fix().pFun(i)
                        else -> just(pa toT i)
                    }
                }
            }
        })
}

// TODO move to seperate lib
// kotlin parser combinator
// TODO better error reporting
data class State<I>(val input: I, val offset: Int) {
    fun longestMatch(s: State<I>): State<I> =
        if (offset <= s.offset) s
        else this

    companion object
}

sealed class ParsecError<E, T> {
    data class Trivial<E, T>(
        val offset: Int,
        val unexpectedTokens: Option<ErrorItem<T>>,
        val expectedTokens: Set<ErrorItem<T>>
    ) : ParsecError<E, T>()

    fun toHints(currOff: Int): Hints<T> = when (this) {
        is Trivial ->
            if (currOff == offset) Hints(if (expectedTokens.isEmpty()) emptyList() else listOf(expectedTokens))
            else Hints.empty()
    }

    fun withHints(h: Hints<T>): ParsecError<E, T> = when (this) {
        is Trivial -> Trivial(
            offset,
            unexpectedTokens,
            expectedTokens.union(h.hints.foldLeft(emptySet()) { acc, v -> acc.union(v) })
        )
    }

    fun offset(): Int = when (this) {
        is Trivial -> offset
    }

    operator fun plus(other: ParsecError<E, T>): ParsecError<E, T> {
        val lOff = offset()
        val rOff = other.offset()

        return if (lOff < rOff) other
        else if (lOff > rOff) this
        else when (this) {
            is Trivial -> when (other) {
                is Trivial -> Trivial(
                    offset,
                    unexpectedTokens.or(other.unexpectedTokens),
                    expectedTokens.union(other.expectedTokens)
                )
            }
        }
    }
}

// Input typeclass to provide options to implement this for other text types (and streaming text)
/**
 * S is the input type
 * EL a single element of the stream
 * CHUNK a single chunk of the stream
 */
interface Stream<S, EL, CHUNK> {
    fun EQEL(): Eq<EL>
    fun EQCHUNK(): Eq<CHUNK>

    fun S.takeOne(): Option<Tuple2<EL, S>>
    fun S.takeWhile(p: (EL) -> Boolean): Tuple2<CHUNK, S>
    fun S.take(i: Int): Option<Tuple2<CHUNK, S>>

    fun CHUNK.isEmpty(): Boolean
    fun CHUNK.size(): Int

    fun List<EL>.toChunk(): CHUNK
    fun CHUNK.toTokens(): List<EL>
}

// TODO monadplus
interface MonadParsec<E, I, EL, CHUNK, M> : Monad<M>, Alternative<M> {
    fun SI(): Stream<I, EL, CHUNK>

    fun <A> Kind<M, A>.label(str: String): Kind<M, A>

    fun <A> Kind<M, A>.hidden(): Kind<M, A> = label("")

    fun <A> Kind<M, A>.tryP(): Kind<M, A>

    fun <A> Kind<M, A>.lookAhead(): Kind<M, A>

    fun <A> Kind<M, A>.notFollowedBy(): Kind<M, Unit>

    fun <A> Kind<M, A>.withRecovery(h: (ParsecError<E, EL>) -> Kind<M, A>): Kind<M, A>

    fun <A> Kind<M, A>.observing(): Kind<M, Either<ParsecError<E, EL>, A>>

    fun eof(): Kind<M, Unit>

    fun <A> token(expected: Set<ErrorItem<EL>>, matcher: (EL) -> Option<A>): Kind<M, A>

    fun tokens(chunk: CHUNK): Kind<M, CHUNK>

    fun takeWhile(label: Option<String>, matcher: (EL) -> Boolean): Kind<M, CHUNK>

    fun takeAtLeastOneWhile(label: Option<String>, matcher: (EL) -> Boolean): Kind<M, CHUNK>

    fun take(label: Option<String>, n: Int): Kind<M, CHUNK>

    fun getParserState(): Kind<M, State<I>>

    fun updateParserState(f: (State<I>) -> State<I>): Kind<M, Unit>

    fun <A> Kind<M, A>.optional(): Kind<M, Option<A>> = map { it.some() }.orElse(just(None))
}

// combinators
fun <E, I, EL, CHUNK, M> MonadParsec<E, I, EL, CHUNK, M>.single(el: EL): Kind<M, EL> =
    token(setOf(ErrorItem.Tokens(Nel(el)))) { it.some().filter { SI().EQEL().run { el.eqv(it) } } }

fun <E, I, EL, CHUNK, M> MonadParsec<E, I, EL, CHUNK, M>.satisfy(p: (EL) -> Boolean): Kind<M, EL> =
    token(emptySet()) { it.some().filter(p) }

fun <E, I, EL, CHUNK, M> MonadParsec<E, I, EL, CHUNK, M>.anySingle(): Kind<M, EL> = satisfy { true }

fun <E, I, EL, CHUNK, M> MonadParsec<E, I, EL, CHUNK, M>.anySingleBut(el: EL): Kind<M, EL> =
    satisfy { SI().EQEL().run { el.neqv(it) } }

fun <F, E, I, EL, CHUNK, M> MonadParsec<E, I, EL, CHUNK, M>.oneOf(cs: Kind<F, EL>, FF: Foldable<F>): Kind<M, EL> =
    satisfy { c -> FF.run { cs.exists { SI().EQEL().run { c.eqv(it) } } } }

fun <F, E, I, EL, CHUNK, M> MonadParsec<E, I, EL, CHUNK, M>.noneOf(cs: Kind<F, EL>, FF: Foldable<F>): Kind<M, EL> =
    satisfy { c -> FF.run { cs.forAll { SI().EQEL().run { c.neqv(it) } } } }

fun <E, I, EL, CHUNK, M> MonadParsec<E, I, EL, CHUNK, M>.chunk(chunk: CHUNK): Kind<M, CHUNK> =
    tokens(chunk)

fun <E, I, EL, CHUNK, M> MonadParsec<E, I, EL, CHUNK, M>.takeRemaining(): Kind<M, CHUNK> =
    takeWhile(None) { true }

