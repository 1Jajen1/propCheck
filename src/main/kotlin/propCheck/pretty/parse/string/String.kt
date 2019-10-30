package propCheck.pretty.parse.string

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.eq
import arrow.core.extensions.list.foldable.foldLeft
import arrow.typeclasses.Eq
import propCheck.pretty.parse.*

interface StringStream : Stream<String, Char, String> {
    override fun EQCHUNK(): Eq<String> = String.eq()
    override fun EQEL(): Eq<Char> = Char.eq()

    override fun String.isEmpty(): Boolean = length == 0
    override fun String.size(): Int = length

    override fun String.take(i: Int): Option<Tuple2<String, String>> = when {
        i <= 0 -> ("" toT this).some()
        length == 0 -> None
        else -> (this.substring(0, i - 1) toT this.substring(i)).some()
    }

    override fun String.takeOne(): Option<Tuple2<Char, String>> =
        if (length == 0) None
        else (first() toT drop(1)).some()

    override fun String.takeWhile(p: (Char) -> Boolean): Tuple2<String, String> =
        takeWhile_(p).let { match -> (match toT this.substring(match.length)) }

    override fun List<Char>.toChunk(): String = String(toCharArray())
    override fun String.toTokens(): List<Char> = toCharArray().toList()
}

fun String.Companion.stream(): Stream<String, Char, String> = object : StringStream {}

private inline fun String.takeWhile_(p: (Char) -> Boolean): String =
    takeWhile(p)

// combinators
fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.newline(): Kind<M, Char> =
    char('\n')

fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.crlf(): Kind<M, CHUNK> =
    string(SI().run { listOf('\r', '\n').toChunk() })

fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.eol(): Kind<M, CHUNK> =
    newline()
        .map { SI().run { listOf(it).toChunk() } }.orElse(crlf())

fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.tab(): Kind<M, Char> =
    char('\t')

fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.space(): Kind<M, Unit> =
    takeWhile { it.isWhitespace() }.unit()


fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.atLeastOneSpace(): Kind<M, Unit> =
    takeAtLeastOneWhile { it.isWhitespace() }.unit()

fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.controlChar(): Kind<M, Char> =
    satisfy { it.isISOControl() }

fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.spaceChar(): Kind<M, Char> =
    satisfy { it.isWhitespace() }

fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.upperChar(): Kind<M, Char> =
    satisfy { it.isUpperCase() }

fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.lowerChar(): Kind<M, Char> =
    satisfy { it.isLowerCase() }

fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.letterChar(): Kind<M, Char> =
    satisfy { it.isLetter() }

fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.digitChar(): Kind<M, Char> =
    satisfy { it.isDigit() }

fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.char(c: Char): Kind<M, Char> =
    single(c)

fun <E, I, EL, CHUNK, M> MonadParsec<E, I, EL, CHUNK, M>.string(str: CHUNK): Kind<M, CHUNK> =
    chunk(str)

// TODO kotlin has inbuilt support for these, so if we fix CHUNK to String we get this for free
fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.decimal(): Kind<M, Long> =
    takeAtLeastOneWhile { it in '0'..'9' }.map { chunk ->
        SI().run {
            chunk.toTokens().foldLeft(0L) { acc, v ->
                acc * 10L + v.toString().toInt(10)
            }
        }
    }

fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.binary(): Kind<M, Long> =
    takeAtLeastOneWhile { it == '0' || it == '1' }.map { chunk ->
        SI().run {
            chunk.toTokens().foldLeft(0L) { acc, v ->
                acc * 2L + v.toString().toInt(2)
            }
        }
    }


fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.octal(): Kind<M, Long> =
    takeAtLeastOneWhile { it in '0'..'7' }.map { chunk ->
        SI().run {
            chunk.toTokens().foldLeft(0L) { acc, v ->
                acc * 2L + v.toString().toInt(8)
            }
        }
    }

fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.hexadecimal(): Kind<M, Long> =
    takeAtLeastOneWhile { it.isHexDigit() }.map { chunk ->
        SI().run {
            chunk.toTokens().foldLeft(0L) { acc, v ->
                acc * 16L + v.toString().toInt(16)
            }
        }
    }

fun Char.isHexDigit(): Boolean = this in ('0'..'9') || this in ('A'..'F') || this in ('a'..'f')

fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.signedLong(p: Kind<M, Long>): Kind<M, Long> =
    (char('+').map { { l: Long -> l } }.orElse(char('-').map { { l: Long -> l * (-1) } }))
        .optional()
        .flatMap { optF ->
            p.map { optF.fold({ { l: Long -> l } }, ::identity)(it) }
        }

fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.signedDouble(p: Kind<M, Double>): Kind<M, Double> =
    (char('+').map { { l: Double -> l } }.orElse(char('-').map { { l: Double -> l * (-1) } }))
        .optional()
        .flatMap { optF ->
            p.map { optF.fold({ { l: Double -> l } }, ::identity)(it) }
        }

fun <E, I, CHUNK, M> MonadParsec<E, I, Char, CHUNK, M>.double(): Kind<M, Double> =
    fx.monad {
        val (fst) = decimal()
        !char('.')
        // this is not quite correct but good enough for now, fix this later
        val (snd) = decimal()

        // I don't like this
        "$fst.$snd".toDouble()
    }

