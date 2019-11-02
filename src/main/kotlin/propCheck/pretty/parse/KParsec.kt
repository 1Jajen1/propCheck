package propCheck.pretty.parse

import arrow.Kind
import arrow.Kind3
import arrow.core.*
import arrow.extension
import arrow.typeclasses.*
import propCheck.pretty.parse.parsect.alternative.alternative

// TODO move to seperate lib
// kotlin parser combinator
// TODO better error reporting
fun <E, I, M, A> ParsecT<E, I, M, A>.runParsecT(input: State<I>, MM: Monad<M>): Kind<M, Either<ParsecError<E>, A>> {
    fun consumedOk(a: A, i: State<I>): Kind<M, Either<ParsecError<E>, A>> = MM.just(a.right())
    fun consumedErr(e: ParsecError<E>, i: State<I>): Kind<M, Either<ParsecError<E>, A>> = MM.just(e.left())
    fun emptyOk(a: A, i: State<I>): Kind<M, Either<ParsecError<E>, A>> = MM.just(a.right())
    fun emptyErr(e: ParsecError<E>, i: State<I>): Kind<M, Either<ParsecError<E>, A>> = MM.just(e.left())

    return runParser(input, ::consumedOk, ::consumedErr, ::emptyOk, ::emptyErr)
}

data class State<I>(val input: I, val offset: Int) {
    companion object
}

class ForParsecT private constructor()
typealias ParsecTOf<E, I, M, A> = Kind<ParsecTPartialOf<E, I, M>, A>
typealias ParsecTPartialOf<E, I, M> = Kind3<ForParsecT, E, I, M>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <E, I, M, A> ParsecTOf<E, I, M, A>.fix(): ParsecT<E, I, M, A> =
    this as ParsecT<E, I, M, A>

typealias Parsec<E, I, A> = ParsecT<E, I, ForId, A>

interface ParsecT<E, I, M, A> : ParsecTOf<E, I, M, A> {
    fun <Z> runParser(
        state: State<I>,
        consumedOk: (A, State<I>) -> Kind<M, Z>,
        consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
        emptyOk: (A, State<I>) -> Kind<M, Z>,
        emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
    ): Kind<M, Z>

    fun <B> map(f: (A) -> B): ParsecT<E, I, M, B> = object : ParsecT<E, I, M, B> {
        override fun <Z> runParser(
            state: State<I>,
            consumedOk: (B, State<I>) -> Kind<M, Z>,
            consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
            emptyOk: (B, State<I>) -> Kind<M, Z>,
            emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
        ): Kind<M, Z> =
            this@ParsecT.runParser(
                state,
                { a, i -> consumedOk(f(a), i) },
                consumedError,
                { a, i -> emptyOk(f(a), i) },
                emptyError
            )
    }

    fun <B> ap(ff: ParsecT<E, I, M, (A) -> B>): ParsecT<E, I, M, B> = object : ParsecT<E, I, M, B> {
        override fun <Z> runParser(
            state: State<I>,
            consumedOk: (B, State<I>) -> Kind<M, Z>,
            consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
            emptyOk: (B, State<I>) -> Kind<M, Z>,
            emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
        ): Kind<M, Z> {
            fun succ(a: A, i: State<I>): Kind<M, Z> {
                return ff.runParser(
                    i,
                    { f, i -> consumedOk(f(a), i) },
                    consumedError,
                    { f, i -> consumedOk(f(a), i) },
                    emptyError
                )
            }

            fun empt(a: A, i: State<I>): Kind<M, Z> {
                return ff.runParser(
                    i,
                    { f, i -> consumedOk(f(a), i) },
                    consumedError,
                    { f, i -> emptyOk(f(a), i) },
                    emptyError
                )
            }

            return this@ParsecT.runParser(state, ::succ, consumedError, ::empt, emptyError)
        }
    }

    fun <B> lazyAp(ff: Eval<ParsecT<E, I, M, (A) -> B>>): ParsecT<E, I, M, B> = object : ParsecT<E, I, M, B> {
        override fun <Z> runParser(
            state: State<I>,
            consumedOk: (B, State<I>) -> Kind<M, Z>,
            consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
            emptyOk: (B, State<I>) -> Kind<M, Z>,
            emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
        ): Kind<M, Z> {
            fun succ(a: A, i: State<I>): Kind<M, Z> {
                return ff.value().runParser(
                    i,
                    { f, i -> consumedOk(f(a), i) },
                    consumedError,
                    { f, i -> consumedOk(f(a), i) },
                    emptyError
                )
            }

            fun empt(a: A, i: State<I>): Kind<M, Z> {
                return ff.value().runParser(
                    i,
                    { f, i -> consumedOk(f(a), i) },
                    consumedError,
                    { f, i -> emptyOk(f(a), i) },
                    emptyError
                )
            }

            return this@ParsecT.runParser(state, ::succ, consumedError, ::empt, emptyError)
        }
    }

    fun <B> flatMap(f: (A) -> ParsecT<E, I, M, B>): ParsecT<E, I, M, B> = object : ParsecT<E, I, M, B> {
        override fun <Z> runParser(
            state: State<I>,
            consumedOk: (B, State<I>) -> Kind<M, Z>,
            consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
            emptyOk: (B, State<I>) -> Kind<M, Z>,
            emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
        ): Kind<M, Z> {
            fun succ(a: A, i: State<I>): Kind<M, Z> {
                return f(a).runParser(
                    i,
                    { b, i -> consumedOk(b, i) },
                    consumedError,
                    { b, i -> consumedOk(b, i) },
                    emptyError
                )
            }

            fun empt(a: A, i: State<I>): Kind<M, Z> {
                return f(a).runParser(
                    i,
                    { b, i -> consumedOk(b, i) },
                    consumedError,
                    { b, i -> emptyOk(b, i) },
                    emptyError
                )
            }
            return this@ParsecT.runParser(state, ::succ, consumedError, ::empt, emptyError)
        }
    }

    fun orElse(other: ParsecT<E, I, M, A>): ParsecT<E, I, M, A> = object : ParsecT<E, I, M, A> {
        override fun <Z> runParser(
            state: State<I>,
            consumedOk: (A, State<I>) -> Kind<M, Z>,
            consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
            emptyOk: (A, State<I>) -> Kind<M, Z>,
            emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
        ): Kind<M, Z> {
            // This is trivial stuff atm, for better errors change this
            // TODO combine errors and provide combined input
            fun emptErr(e: ParsecError<E>, inp: State<I>): Kind<M, Z> =
                other.runParser(state, consumedOk, consumedError, emptyOk, emptyError)
            return this@ParsecT.runParser(state, consumedOk, consumedError, emptyOk, ::emptErr)
        }
    }

    fun some(): ParsecT<E, I, M, Sequence<A>> = object : ParsecT<E, I, M, Sequence<A>> {
        override fun <Z> runParser(
            state: State<I>,
            consumedOk: (Sequence<A>, State<I>) -> Kind<M, Z>,
            consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
            emptyOk: (Sequence<A>, State<I>) -> Kind<M, Z>,
            emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
        ): Kind<M, Z> = ParsecT.alternative<E, I, M>().run {
            this@ParsecT.lazyAp(Eval.later {
                this@ParsecT.many().map { seq ->
                    { el: A -> sequenceOf(el) + seq }
                }.fix()
            })
        }.runParser(state, consumedOk, consumedError, emptyOk, emptyError)
    }

    companion object {
        fun <E, I, M, A> just(a: A): ParsecT<E, I, M, A> = object : ParsecT<E, I, M, A> {
            override fun <Z> runParser(
                state: State<I>,
                consumedOk: (A, State<I>) -> Kind<M, Z>,
                consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
                emptyOk: (A, State<I>) -> Kind<M, Z>,
                emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
            ): Kind<M, Z> = emptyOk(a, state)
        }

        fun <E, I, M, A> empty(): ParsecT<E, I, M, A> = object : ParsecT<E, I, M, A> {
            override fun <Z> runParser(
                state: State<I>,
                consumedOk: (A, State<I>) -> Kind<M, Z>,
                consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
                emptyOk: (A, State<I>) -> Kind<M, Z>,
                emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
            ): Kind<M, Z> = emptyError(ParsecError.Trivial(), state)
        }
    }
}

sealed class ParsecError<E> {
    class Trivial<E> : ParsecError<E>()
}

@extension
interface ParsecFunctor<E, I, M> : Functor<ParsecTPartialOf<E, I, M>> {
    override fun <A, B> Kind<ParsecTPartialOf<E, I, M>, A>.map(f: (A) -> B): Kind<ParsecTPartialOf<E, I, M>, B> =
        fix().map(f)
}

@extension
interface ParsecApplicative<E, I, M> : Applicative<ParsecTPartialOf<E, I, M>> {
    override fun <A> just(a: A): Kind<ParsecTPartialOf<E, I, M>, A> = ParsecT.just(a)
    override fun <A, B> Kind<ParsecTPartialOf<E, I, M>, A>.ap(ff: Kind<ParsecTPartialOf<E, I, M>, (A) -> B>): Kind<ParsecTPartialOf<E, I, M>, B> =
        fix().ap(ff.fix())
}

@extension
interface ParsecMonad<E, I, M> : Monad<ParsecTPartialOf<E, I, M>> {
    override fun <A> just(a: A): Kind<ParsecTPartialOf<E, I, M>, A> = ParsecT.just(a)
    override fun <A, B> Kind<ParsecTPartialOf<E, I, M>, A>.flatMap(f: (A) -> Kind<ParsecTPartialOf<E, I, M>, B>): Kind<ParsecTPartialOf<E, I, M>, B> =
        fix().flatMap(f andThen { it.fix() })

    override fun <A, B> tailRecM(
        a: A,
        f: (A) -> Kind<ParsecTPartialOf<E, I, M>, Either<A, B>>
    ): Kind<ParsecTPartialOf<E, I, M>, B> = f(a).flatMap { it.fold({ tailRecM(it, f) }, { just(it) }) }
}

@extension
interface ParsecAlternative<E, I, M> : Alternative<ParsecTPartialOf<E, I, M>>, ParsecApplicative<E, I, M> {
    override fun <A> empty(): Kind<ParsecTPartialOf<E, I, M>, A> = ParsecT.empty()
    override fun <A> Kind<ParsecTPartialOf<E, I, M>, A>.orElse(b: Kind<ParsecTPartialOf<E, I, M>, A>): Kind<ParsecTPartialOf<E, I, M>, A> =
        fix().orElse(b.fix())

    override fun <A> Kind<ParsecTPartialOf<E, I, M>, A>.some(): Kind<ParsecTPartialOf<E, I, M>, SequenceK<A>> =
        fix().some().map { it.k() }

    // TODO?
    override fun <A> Kind<ParsecTPartialOf<E, I, M>, A>.combineK(y: Kind<ParsecTPartialOf<E, I, M>, A>): Kind<ParsecTPartialOf<E, I, M>, A> =
        fix().orElse(y)
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

// --------------- primitives
fun <E, I, M, A> ParsecT<E, I, M, A>.ptryP(): ParsecT<E, I, M, A> = object : ParsecT<E, I, M, A> {
    override fun <Z> runParser(
        state: State<I>,
        consumedOk: (A, State<I>) -> Kind<M, Z>,
        consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
        emptyOk: (A, State<I>) -> Kind<M, Z>,
        emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
    ): Kind<M, Z> {
        fun nEmptErr(err: ParsecError<E>, ignored: State<I>): Kind<M, Z> = emptyError(err, state)
        return this@ptryP.runParser(state, consumedOk, consumedError, emptyOk, ::nEmptErr)
    }
}

fun <E, I, M, A> ParsecT<E, I, M, A>.plookAhead(): ParsecT<E, I, M, A> = object : ParsecT<E, I, M, A> {
    override fun <Z> runParser(
        state: State<I>,
        consumedOk: (A, State<I>) -> Kind<M, Z>,
        consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
        emptyOk: (A, State<I>) -> Kind<M, Z>,
        emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
    ): Kind<M, Z> {
        fun nEmptOk(a: A, ignored: State<I>): Kind<M, Z> = emptyOk(a, state)

        return this@plookAhead.runParser(state, ::nEmptOk, consumedError, ::nEmptOk, emptyError)
    }
}

fun <E, I, M, A> ParsecT<E, I, M, A>.pnotFollowedBy(): ParsecT<E, I, M, Unit> = object : ParsecT<E, I, M, Unit> {
    override fun <Z> runParser(
        state: State<I>,
        consumedOk: (Unit, State<I>) -> Kind<M, Z>,
        consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
        emptyOk: (Unit, State<I>) -> Kind<M, Z>,
        emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
    ): Kind<M, Z> {
        // TODO better errors
        fun nConOk(a: A, i: State<I>): Kind<M, Z> = emptyError(ParsecError.Trivial(), state)

        fun nEmptOk(a: A, i: State<I>): Kind<M, Z> = emptyError(ParsecError.Trivial(), state)
        fun nConErr(e: ParsecError<E>, i: State<I>): Kind<M, Z> = emptyOk(Unit, state)
        fun nEmptErr(e: ParsecError<E>, i: State<I>): Kind<M, Z> = emptyOk(Unit, state)

        return this@pnotFollowedBy.runParser(state, ::nConOk, ::nConErr, ::nEmptOk, ::nEmptErr)
    }
}

fun <E, I, M, A> ParsecT<E, I, M, A>.pwithRecovery(h: (ParsecError<E>) -> ParsecT<E, I, M, A>): ParsecT<E, I, M, A> =
    object : ParsecT<E, I, M, A> {
        override fun <Z> runParser(
            state: State<I>,
            consumedOk: (A, State<I>) -> Kind<M, Z>,
            consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
            emptyOk: (A, State<I>) -> Kind<M, Z>,
            emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
        ): Kind<M, Z> {
            // TODO better error reporting
            fun nConErr(e: ParsecError<E>, i: State<I>): Kind<M, Z> =
                h(e).runParser(i, consumedOk, consumedError, emptyOk, consumedError)

            fun nEmptErr(e: ParsecError<E>, i: State<I>): Kind<M, Z> =
                h(e).runParser(i, consumedOk, emptyError, emptyOk, emptyError)

            return this@pwithRecovery.runParser(state, consumedOk, ::nConErr, emptyOk, ::nEmptErr)
        }
    }

fun <E, I, M, A> ParsecT<E, I, M, A>.pobserving(): ParsecT<E, I, M, Either<ParsecError<E>, A>> =
    object : ParsecT<E, I, M, Either<ParsecError<E>, A>> {
        override fun <Z> runParser(
            state: State<I>,
            consumedOk: (Either<ParsecError<E>, A>, State<I>) -> Kind<M, Z>,
            consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
            emptyOk: (Either<ParsecError<E>, A>, State<I>) -> Kind<M, Z>,
            emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
        ): Kind<M, Z> {
            return this@pobserving.runParser(
                state,
                { a, i -> consumedOk(a.right(), i) },
                { e, i -> consumedOk(e.left(), i) },
                { a, i -> emptyOk(a.right(), i) },
                { e, i -> emptyOk(e.left(), i) }
            )
        }
    }

fun <E, I, EL, CHUNK, M> peof(stream: Stream<I, EL, CHUNK>): ParsecT<E, I, M, Unit> = object : ParsecT<E, I, M, Unit> {
    override fun <Z> runParser(
        state: State<I>,
        consumedOk: (Unit, State<I>) -> Kind<M, Z>,
        consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
        emptyOk: (Unit, State<I>) -> Kind<M, Z>,
        emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
    ): Kind<M, Z> = stream.run {
        state.input.takeOne().fold({ emptyOk(Unit, state) }, { (_, _) ->
            // TODO error reporting
            emptyError(ParsecError.Trivial(), state)
        })
    }
}

fun <E, I, EL, CHUNK, M, A> ptoken(matcher: (EL) -> Option<A>, stream: Stream<I, EL, CHUNK>): ParsecT<E, I, M, A> =
    object : ParsecT<E, I, M, A> {
        override fun <Z> runParser(
            state: State<I>,
            consumedOk: (A, State<I>) -> Kind<M, Z>,
            consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
            emptyOk: (A, State<I>) -> Kind<M, Z>,
            emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
        ): Kind<M, Z> = stream.run {
            state.input.takeOne().fold({
                // TODO error
                emptyError(ParsecError.Trivial(), state)
            }, { (tok, rem) ->
                matcher(tok).fold({
                    // TODO error
                    emptyError(ParsecError.Trivial(), state)
                }, { a ->
                    consumedOk(a, State(rem, state.offset + 1))
                })
            })
        }
    }

fun <E, I, EL, CHUNK, M> ptokens(chunk: CHUNK, stream: Stream<I, EL, CHUNK>): ParsecT<E, I, M, CHUNK> =
    object : ParsecT<E, I, M, CHUNK> {
        override fun <Z> runParser(
            state: State<I>,
            consumedOk: (CHUNK, State<I>) -> Kind<M, Z>,
            consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
            emptyOk: (CHUNK, State<I>) -> Kind<M, Z>,
            emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
        ): Kind<M, Z> = stream.run {
            state.input.take(chunk.size()).fold({
                // TODO errors
                emptyError(ParsecError.Trivial(), state)
            }, { (cs, rem) ->
                // TODO err
                if (EQCHUNK().run { cs.eqv(chunk) }) State(rem, state.offset + cs.size()).let { newState ->
                    if (chunk.isEmpty()) emptyOk(cs, newState)
                    else consumedOk(cs, newState)
                } else emptyError(ParsecError.Trivial(), state)
            })
        }
    }

fun <E, I, EL, CHUNK, M> ptakeWhile(matcher: (EL) -> Boolean, stream: Stream<I, EL, CHUNK>): ParsecT<E, I, M, CHUNK> =
    object : ParsecT<E, I, M, CHUNK> {
        override fun <Z> runParser(
            state: State<I>,
            consumedOk: (CHUNK, State<I>) -> Kind<M, Z>,
            consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
            emptyOk: (CHUNK, State<I>) -> Kind<M, Z>,
            emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
        ): Kind<M, Z> = stream.run {
            state.input.takeWhile(matcher).let { (res, rem) ->
                State(rem, state.offset + res.size()).let { newState ->
                    if (res.isEmpty()) emptyOk(res, newState)
                    else consumedOk(res, newState)
                }
            }
        }
    }

fun <E, I, EL, CHUNK, M> ptakeAtLeastOneWhile(
    matcher: (EL) -> Boolean,
    stream: Stream<I, EL, CHUNK>
): ParsecT<E, I, M, CHUNK> = object : ParsecT<E, I, M, CHUNK> {
    override fun <Z> runParser(
        state: State<I>,
        consumedOk: (CHUNK, State<I>) -> Kind<M, Z>,
        consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
        emptyOk: (CHUNK, State<I>) -> Kind<M, Z>,
        emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
    ): Kind<M, Z> = stream.run {
        state.input.takeWhile(matcher).let { (res, rem) ->
            // TODO errors!
            if (res.isEmpty()) emptyError(ParsecError.Trivial(), state)
            else consumedOk(res, State(rem, state.offset + res.size()))
        }
    }
}

fun <E, I, EL, CHUNK, M> ptake(n: Int, stream: Stream<I, EL, CHUNK>): ParsecT<E, I, M, CHUNK> =
    object : ParsecT<E, I, M, CHUNK> {
        override fun <Z> runParser(
            state: State<I>,
            consumedOk: (CHUNK, State<I>) -> Kind<M, Z>,
            consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
            emptyOk: (CHUNK, State<I>) -> Kind<M, Z>,
            emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
        ): Kind<M, Z> = stream.run {
            state.input.take(n).fold({
                // TODO err
                emptyError(ParsecError.Trivial(), state)
            }, { (chunk, rem) ->
                // TODO err
                if (chunk.size() != n) emptyError(ParsecError.Trivial(), state)
                else consumedOk(chunk, State(rem, state.offset + chunk.size()))
            })
        }
    }

fun <E, I, M> pgetParserState(): ParsecT<E, I, M, State<I>> = object : ParsecT<E, I, M, State<I>> {
    override fun <Z> runParser(
        state: State<I>,
        consumedOk: (State<I>, State<I>) -> Kind<M, Z>,
        consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
        emptyOk: (State<I>, State<I>) -> Kind<M, Z>,
        emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
    ): Kind<M, Z> = emptyOk(state, state)
}

fun <E, I, M> pupdateParserState(f: (State<I>) -> State<I>): ParsecT<E, I, M, Unit> = object : ParsecT<E, I, M, Unit> {
    override fun <Z> runParser(
        state: State<I>,
        consumedOk: (Unit, State<I>) -> Kind<M, Z>,
        consumedError: (ParsecError<E>, State<I>) -> Kind<M, Z>,
        emptyOk: (Unit, State<I>) -> Kind<M, Z>,
        emptyError: (ParsecError<E>, State<I>) -> Kind<M, Z>
    ): Kind<M, Z> = emptyOk(Unit, f(state))
}

// TODO monadplus
interface MonadParsec<E, I, EL, CHUNK, M> : Monad<M>, Alternative<M> {
    fun SI(): Stream<I, EL, CHUNK>

    fun <A> Kind<M, A>.tryP(): Kind<M, A>

    fun <A> Kind<M, A>.lookAhead(): Kind<M, A>

    fun <A> Kind<M, A>.notFollowedBy(): Kind<M, Unit>

    fun <A> Kind<M, A>.withRecovery(h: (ParsecError<E>) -> Kind<M, A>): Kind<M, A>

    fun <A> Kind<M, A>.observing(): Kind<M, Either<ParsecError<E>, A>>

    fun eof(): Kind<M, Unit>

    fun <A> token(matcher: (EL) -> Option<A>): Kind<M, A>

    fun tokens(chunk: CHUNK): Kind<M, CHUNK>

    fun takeWhile(matcher: (EL) -> Boolean): Kind<M, CHUNK>

    fun takeAtLeastOneWhile(matcher: (EL) -> Boolean): Kind<M, CHUNK>

    fun take(n: Int): Kind<M, CHUNK>

    fun getParserState(): Kind<M, State<I>>

    fun updateParserState(f: (State<I>) -> State<I>): Kind<M, Unit>

    fun <A> Kind<M, A>.optional(): Kind<M, Option<A>>
}

// TODO why is this not generated?
fun <E, I, EL, CHUNK, M> ParsecT.Companion.monadParsec(stream: Stream<I, EL, CHUNK>): ParsecTMonadParsec<E, I, EL, CHUNK, M> =
    object : ParsecTMonadParsec<E, I, EL, CHUNK, M> {
        override fun SI(): Stream<I, EL, CHUNK> = stream
    }

@extension
interface ParsecTMonadParsec<E, I, EL, CHUNK, M> : MonadParsec<E, I, EL, CHUNK, ParsecTPartialOf<E, I, M>>,
    ParsecMonad<E, I, M>, ParsecAlternative<E, I, M> {

    override fun SI(): Stream<I, EL, CHUNK>

    override fun <A, B> Kind<ParsecTPartialOf<E, I, M>, A>.ap(ff: Kind<ParsecTPartialOf<E, I, M>, (A) -> B>): Kind<ParsecTPartialOf<E, I, M>, B> =
        fix().ap(ff.fix())

    override fun <A> just(a: A): Kind<ParsecTPartialOf<E, I, M>, A> = ParsecT.just(a)

    override fun eof(): Kind<ParsecTPartialOf<E, I, M>, Unit> = peof(SI())
    override fun <A> Kind<ParsecTPartialOf<E, I, M>, A>.lookAhead(): Kind<ParsecTPartialOf<E, I, M>, A> =
        fix().plookAhead()

    override fun <A> Kind<ParsecTPartialOf<E, I, M>, A>.notFollowedBy(): Kind<ParsecTPartialOf<E, I, M>, Unit> =
        fix().pnotFollowedBy()

    override fun <A> Kind<ParsecTPartialOf<E, I, M>, A>.observing(): Kind<ParsecTPartialOf<E, I, M>, Either<ParsecError<E>, A>> =
        fix().pobserving()

    override fun take(n: Int): Kind<ParsecTPartialOf<E, I, M>, CHUNK> = ptake(n, SI())
    override fun takeAtLeastOneWhile(matcher: (EL) -> Boolean): Kind<ParsecTPartialOf<E, I, M>, CHUNK> =
        ptakeAtLeastOneWhile(matcher, SI())

    override fun takeWhile(matcher: (EL) -> Boolean): Kind<ParsecTPartialOf<E, I, M>, CHUNK> = ptakeWhile(matcher, SI())
    override fun <A> token(matcher: (EL) -> Option<A>): Kind<ParsecTPartialOf<E, I, M>, A> =
        ptoken(matcher, SI())

    override fun <A> Kind<ParsecTPartialOf<E, I, M>, A>.tryP(): Kind<ParsecTPartialOf<E, I, M>, A> =
        fix().ptryP()

    override fun <A> Kind<ParsecTPartialOf<E, I, M>, A>.withRecovery(h: (ParsecError<E>) -> Kind<ParsecTPartialOf<E, I, M>, A>): Kind<ParsecTPartialOf<E, I, M>, A> =
        fix().pwithRecovery(h andThen { it.fix() })

    override fun tokens(chunk: CHUNK): Kind<ParsecTPartialOf<E, I, M>, CHUNK> =
        ptokens(chunk, SI())

    override fun getParserState(): Kind<ParsecTPartialOf<E, I, M>, State<I>> =
        pgetParserState()

    override fun updateParserState(f: (State<I>) -> State<I>): Kind<ParsecTPartialOf<E, I, M>, Unit> =
        pupdateParserState(f)

    // TODO arrow pr
    override fun <A> Kind<ParsecTPartialOf<E, I, M>, A>.optional(): Kind<ParsecTPartialOf<E, I, M>, Option<A>> =
        map { it.some() }.orElse(just(None))
}

// combinators
fun <E, I, EL, CHUNK, M> MonadParsec<E, I, EL, CHUNK, M>.single(el: EL): Kind<M, EL> =
    token { it.some().filter { SI().EQEL().run { el.eqv(it) } } }

fun <E, I, EL, CHUNK, M> MonadParsec<E, I, EL, CHUNK, M>.satisfy(p: (EL) -> Boolean): Kind<M, EL> =
    token { it.some().filter(p) }

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
    takeWhile { true }

