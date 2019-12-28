package propCheck.pretty

import arrow.core.*
import arrow.core.extensions.id.monad.monad
import arrow.extension
import arrow.typeclasses.Eq
import arrow.typeclasses.Show
import pretty.*
import propCheck.pretty.parse.*
import propCheck.pretty.parse.string.*

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
        is KList -> vals.map { it.doc<A>() }.newLineList().group()
        is KTuple -> vals.map { it.doc<A>() }.newLineTupled().group()
        is Record -> name.text<A>() softLineBreak
                (kv.map { (k, v) ->
                    (k.text<A>() spaced equals<A>() + space() + v.doc()).group()
                }).newLineTupled()
                    .nest(name.length)
        is Cons -> name.text<A>() softLineBreak
                (props.map { it.doc<A>().group() })
                    .newLineTupled()
                    .nest(name.length)
                    .group()
    }

    private fun <A> List<Doc<A>>.newLineTupled() = this.encloseSep(
        (lParen<A>() + space()).flatAlt(lParen()),
        (lineBreak<A>() + rParen()).flatAlt(rParen()),
        comma<A>() + space()
    )

    private fun <A> List<Doc<A>>.newLineList() = encloseSep(
        (lBracket<A>() + space()).flatAlt(lBracket()),
        (lineBreak<A>() + rBracket()).flatAlt(rBracket()),
        comma<A>() + space()
    )

    companion object
}

data class Test(val l : Int, val r: Double, val e: List<Test>)

fun main() {
    val c = Test(50, 0.75, emptyList())
    val b = Test(30, 0.5, listOf(c, c, c))
    val a = Test(10, 0.10, listOf(b, b, b, b)).toString().also(::println)

    outputParser().runParsecT(State(a, 0)).fix().value()
        .also(::println)
}

@extension
interface KValueShow : Show<KValue> {
    override fun KValue.show(): String = doc<Nothing>().renderPretty().renderString()
}

@extension
interface KValueEq : Eq<KValue> {
    override fun KValue.eqv(b: KValue): Boolean = this == b
}

typealias Parser<A> = KParsecT<Nothing, String, Char, ForId, A>

fun parser() = KParsecT.monadParsec<Nothing, String, Char, String, ForId>(String.stream(), Id.monad())

// Top level parser
fun outputParser(): Parser<KValue> =
    listParser()
        .orElse(tupleParser())
        .orElse(consParser())
        .orElse(recordParser())
        // .orElse(rawStringParser())

fun valueParser(pred: (Char) -> Boolean): Parser<KValue> = parser().run {
    listParser()
        .orElse(tupleParser())
        .orElse(consParser())
        .orElse(recordParser())
        .orElse(signedDouble(double()).map { KValue.Rational(it) })
        .orElse(signedLong(decimal()).map { KValue.Decimal(it) }).fix()
    // .orElse(stringValueParser(pred)).fix()
}

var c = 0

fun listParser(): Parser<KValue> = parser().run {
    unit().fix().flatMap {
        valueParser { it != ',' && it != ']' }.withSeperator(',').between('[', ']')
            .map { KValue.KList(it.toList()) }
    }.fix()
}

fun consParser(): Parser<KValue> = parser().run {
    takeAtLeastOneWhile(None) { it != '(' && it.isLetter() }.label("constructor name")
        .flatMap { conName -> tupleParser().map { props -> KValue.Cons(conName, (props as KValue.KTuple).vals) }.fix() }.fix()
}

fun recordParser(): Parser<KValue> = parser().run {
    fx.monad {
        val conName = takeAtLeastOneWhile(None) { it != '(' && it.isLetter() }.label("constructor name").bind()
        val props = propertyParser().withSeperator(',').between('(', ')').bind()
        KValue.Record(conName, props.toList())
    }.fix()
}

fun propertyParser(): Parser<Tuple2<String, KValue>> = parser().run {
    fx.monad {
        val propName = takeAtLeastOneWhile(None) { it != '=' }.label("property name").bind()
        val value = char('=').label("equals").fix()
            .flatMap { space().fix() }
            .flatMap { valueParser { it != ',' && it != ')' } }.bind()
        propName toT value
    }.fix()
}

fun <A> Parser<A>.between(start: Char, end: Char): Parser<A> = parser().run {
    fx.monad {
        char(start).label("$start").bind()
        val a = this@between.bind()
        char(end).label("$end").bind()
        a
    }.fix()
}

fun <A> Parser<A>.withSeperator(sep: Char): Parser<SequenceK<A>> = parser().run {
    fx.monad {
        val seq = this@withSeperator.effectM { char(sep).label("$sep").followedBy(space()) }.many().bind()
        val last = this@withSeperator.optional().bind()

        last.fold({ seq }, { (seq + sequenceOf(it)).k() })
    }.fix()
}

fun stringValueParser(pred: (Char) -> Boolean): Parser<KValue> = parser().run {
    takeWhile(None, pred).map { KValue.RawString(it) }.fix()
}

fun tupleParser(): Parser<KValue> = parser().run {
    unit().fix().flatMap {
        valueParser { it != ',' && it != ')' }.withSeperator(',').between('(', ')')
            .map { KValue.KTuple(it.toList()) }
    }.fix()
}

fun rawStringParser(): Parser<KValue> = parser().run {
    takeRemaining().map { KValue.RawString(it) }.fix()
}