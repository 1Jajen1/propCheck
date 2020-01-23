package propCheck.pretty

import arrow.core.*
import arrow.core.extensions.id.monad.monad
import arrow.core.extensions.listk.foldable.foldable
import arrow.extension
import arrow.typeclasses.Eq
import arrow.typeclasses.Show
import arrow.typeclasses.altSum
import kparsec.*
import kparsec.string.*
import pretty.*
import pretty.symbols.*
import kotlin.math.min

sealed class KValue {
    data class RawString(val s: String) : KValue()
    data class Decimal(val l: Long) : KValue()
    data class Rational(val r: Double) : KValue()
    data class KList(val vals: List<KValue>) : KValue()
    data class KTuple(val vals: List<KValue>) : KValue()
    data class Record(val name: String, val kv: List<Tuple2<String, KValue>>) : KValue()
    data class Cons(val name: String, val props: List<KValue>) : KValue()

    fun doc(): Doc<Nothing> = when (this) {
        is RawString -> s.doc()
        is Decimal -> l.doc()
        is Rational -> r.doc()
        is KList -> vals.map { it.doc() }.newLineList().align()
        is KTuple -> vals.map { it.doc() }.newLineTupled().align()
        is Record -> {
            val max = min(10, kv.maxBy { it.a.length }?.a?.length ?: 0)
            name.text() +
                    (kv.map { (k, v) ->
                        (k.text().fillBreak(max).flatAlt(k.text()) spaced equals() spaced v.doc().align()).align()
                    }).newLineTupled().align()
        }
        is Cons -> name.text() softLineBreak
                (props.map { it.doc().align() })
                    .newLineTupled()
                    .align()
    }

    private fun <A> List<Doc<A>>.newLineTupled() =
    // Special case this because here the highest layout for () is actually (\n\n) and in the worst case that will get chosen
        if (isEmpty()) lParen() + rParen()
        else encloseSep(
            lParen() + lineBreak() + (space() + space()).flatAlt(nil()),
            lineBreak() + rParen(),
            comma() + space()
        ).group()

    private fun <A> List<Doc<A>>.newLineList() =
    // Special case this because here the highest layout for [] is actually [\n\n] and in the worst case that will get chosen
        if (isEmpty()) lBracket() + rBracket()
        else encloseSep(
            lBracket() + lineBreak() + (space() + space()).flatAlt(nil()),
            lineBreak() + rBracket(),
            comma() + space()
        ).group()

    companion object
}

fun <A> A.showPretty(SA: Show<A> = Show.any()): Doc<Nothing> = SA.run {
    val str = show()
    outputParser().runParser("", str).fold({
        KValue.RawString(str)
    }, ::identity)
}.doc().group()

@extension
interface KValueEq : Eq<KValue> {
    override fun KValue.eqv(b: KValue): Boolean = this == b
}

typealias Parser<A> = KParsecT<Nothing, String, Char, ForId, A>

fun parser() = KParsecT.monadParsec<Nothing, String, Char, String, ForId>(String.stream(), Id.monad())

// Top level parser
fun outputParser(): Parser<KValue> = parser().run {
    valueParser { true }.k().altSum(this, ListK.foldable()).apTap(eof()).fix()
}

fun valueParser(pred: (Char) -> Boolean): List<Parser<KValue>> = parser().run {
    listOf(
        listParser(),
        recordParser(),
        tupleParser(),
        consParser(),
        signedLong(decimal()).map { KValue.Decimal(it) }.fix(),
        signedDouble(double()).map { KValue.Rational(it) }.fix(),
        stringValueParser(pred)
    ) as List<Parser<KValue>>
}

fun listParser(): Parser<KValue> = parser().run {
    unit().fix().flatMap {
        char('[').followedBy(
            valueParser { it != ',' && it != ']' }.map { it.withSeparator(',').apTap(char(']')).fix() }.k()
                .altSum(this@run, ListK.foldable()).fix()
        ).map { KValue.KList(it.toList()) }.fix()
    }.fix()
}

fun consParser(): Parser<KValue> = parser().run {
    takeAtLeastOneWhile("constructor name".some()) { it != '(' && it.isLetter() }
        .flatMap { conName -> tupleParser().map { props -> KValue.Cons(conName, (props as KValue.KTuple).vals) }.fix() }
        .fix()
}

fun recordParser(): Parser<KValue> = parser().run {
    fx.monad {
        val conName = takeAtLeastOneWhile("constructor name".some()) { it != '(' && it.isLetter() }.bind()
        val props = propertyParser().withSeparator(',').between('(', ')').bind()
        KValue.Record(conName, props.toList())
    }.fix()
}

fun propertyParser(): Parser<Tuple2<String, KValue>> = parser().run {
    fx.monad {
        val propName = takeAtLeastOneWhile("property name".some()) { it != '=' }.bind()
        val value = char('=').label("equals").fix()
            .flatMap { space().fix() }
            .flatMap { valueParser { it != ',' && it != ')' }.k().altSum(this@run, ListK.foldable()).fix() }.bind()
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

fun <A> Parser<A>.withSeparator(sep: Char): Parser<List<A>> = parser().run {
    fx.monad {
        val seq = this@withSeparator.apTap(char(sep).label("$sep").followedBy(space())).many().bind().toList()
        val last = this@withSeparator.optional().bind()

        last.fold({ seq }, { (seq + listOf(it)).k() })
    }.fix()
}

fun stringValueParser(pred: (Char) -> Boolean): Parser<KValue> = parser().run {
    takeAtLeastOneWhile("string value".some(), pred).map { KValue.RawString(it) }.fix()
}

fun tupleParser(): Parser<KValue> = parser().run {
    unit().fix().flatMap {
        char('(').followedBy(
            valueParser { it != ',' && it != ')' }.map { it.withSeparator(',').apTap(char(')')) }.k()
                .altSum(this@run, ListK.foldable()).fix()
        ).map { KValue.KTuple(it.toList()) }.fix()
    }.fix()
}

fun rawStringParser(): Parser<KValue> = parser().run {
    takeRemaining().map { KValue.RawString(it) }.fix()
}