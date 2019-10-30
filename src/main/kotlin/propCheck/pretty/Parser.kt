package propCheck.pretty

import arrow.core.*
import arrow.extension
import arrow.typeclasses.Eq
import arrow.typeclasses.Show
import pretty.*
import propCheck.pretty.parse.ParsecT
import propCheck.pretty.parse.fix
import propCheck.pretty.parse.monadParsec
import propCheck.pretty.parse.string.*
import propCheck.pretty.parse.takeRemaining

sealed class KValue {
    data class RawString(val s: String) : KValue()
    data class Decimal(val l: Long) : KValue()
    data class Rational(val r: Double) : KValue()
    data class KList(val vals: List<KValue>) : KValue()
    data class KTuple(val vals: List<KValue>) : KValue()
    data class Record(val name: String, val kv: List<Tuple2<String, KValue>>) : KValue()
    data class Cons(val name: String, val props: List<KValue>) : KValue()

    fun <A> doc(): Doc<A> = when (this) {
        is RawString -> s.doc()
        is Decimal -> l.doc()
        is Rational -> r.doc()
        is KList -> vals.map { it.doc<A>().group() }.list()
        is KTuple -> vals.map { it.doc<A>().group() }.tupled()
        is Record -> name.text<A>() softLineBreak
                (kv.map { (k, v) ->
                    (k.text<A>() spaced equals<A>() +
                            space<A>() +
                            v.doc<A>()).group()
                })
                    .encloseSep(lParen(), lineBreak<A>() + rParen(), comma<A>() + space())
                    .nest(name.length + 1)
        is Cons -> name.text<A>() softLineBreak
                (props.map { it.doc<A>().group() })
                    .encloseSep(lParen(), lineBreak<A>() + rParen(), comma<A>() + space())
                    .nest(name.length + 1)
    }

    companion object
}

@extension
interface KValueShow : Show<KValue> {
    override fun KValue.show(): String = doc<Nothing>().renderPretty().layout()
}

@extension
interface KValueEq : Eq<KValue> {
    override fun KValue.eqv(b: KValue): Boolean = this == b
}

typealias Parser<A> = ParsecT<Nothing, String, ForId, A>

fun parser() = ParsecT.monadParsec<Nothing, String, Char, String, ForId>(String.stream())

// Top level parser
fun outputParser(): Parser<KValue> =
    listParser()
        .orElse(tupleParser())
        .orElse(consParser())
        .orElse(recordParser())
        .orElse(rawStringParser())

fun valueParser(pred: (Char) -> Boolean): Parser<KValue> = parser().run {
    listParser()
        .orElse(tupleParser())
        .orElse(consParser())
        .orElse(recordParser())
        .orElse(signedDouble(double()).map { KValue.Rational(it) })
        .orElse(signedLong(decimal()).map { KValue.Decimal(it) })
        .orElse(stringValueParser(pred)).fix()
}

fun listParser(): Parser<KValue> = parser().run {
    unit().fix().lazyFlatMap {
        Eval.later {
            valueParser { it != ',' && it != ']' }.withSeperator(',').between('[', ']')
                .map { KValue.KList(it.toList()) }
        }
    }.fix()
}

fun consParser(): Parser<KValue> = parser().run {
    fx.monad {
        val (conName) = takeAtLeastOneWhile { it != '(' && it.isLetter() }
        val props = (tupleParser().bind() as KValue.KTuple).vals
        KValue.Cons(conName, props)
    }.fix()
}

fun recordParser(): Parser<KValue> = parser().run {
    fx.monad {
        val (conName) = takeAtLeastOneWhile { it != '(' && it.isLetter() }
        val (props) = propertyParser().withSeperator(',').between('(', ')')
        KValue.Record(conName, props.toList())
    }.fix()
}

fun propertyParser(): Parser<Tuple2<String, KValue>> = parser().run {
    fx.monad {
        val (propName) = takeAtLeastOneWhile { it != '=' }
        val (value) = char('=').fix()
            .lazyFlatMap { Eval.later { valueParser { it != ',' && it != ')' } } }
        propName toT value
    }.fix()
}

fun <A> Parser<A>.between(start: Char, end: Char): Parser<A> = parser().run {
    fx.monad {
        !char(start)
        val a = !this@between
        !char(end)
        a
    }.fix()
}

fun <A> Parser<A>.withSeperator(sep: Char): Parser<SequenceK<A>> = parser().run {
    fx.monad {
        val (seq) = this@withSeperator.effectM { char(sep).followedBy(space()) }.many()
        val (last) = this@withSeperator.optional()

        last.fold({ seq }, { (seq + sequenceOf(it)).k() })
    }.fix()
}

fun stringValueParser(pred: (Char) -> Boolean): Parser<KValue> = parser().run {
    takeWhile(pred).map { KValue.RawString(it) }.fix()
}

fun tupleParser(): Parser<KValue> = parser().run {
    unit().fix().lazyFlatMap {
        Eval.later {
            valueParser { it != ',' && it != ')' }.withSeperator(',').between('(', ')')
                .map { KValue.KTuple(it.toList()) }
        }
    }.fix()
}

fun rawStringParser(): Parser<KValue> = parser().run {
    takeRemaining().map { KValue.RawString(it) }.fix()
}