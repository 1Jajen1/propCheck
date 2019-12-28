package propCheck.pretty

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.fx
import arrow.core.extensions.list.functor.map
import arrow.core.extensions.list.functor.tupleLeft
import arrow.extension
import arrow.recursion.typeclasses.Birecursive
import arrow.syntax.collections.tail
import arrow.typeclasses.Functor
import pretty.*
import propCheck.pretty.kvalue.eq.eq
import propCheck.pretty.kvalue.show.show
import propCheck.pretty.parse.State
import propCheck.pretty.valuediff.birecursive.birecursive
import propCheck.pretty.valuedifff.functor.functor

class ForValueDiffF private constructor()
typealias ValueDiffFOf<F> = Kind<ForValueDiffF, F>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <F> ValueDiffFOf<F>.fix(): ValueDiffF<F> = this as ValueDiffF<F>

sealed class ValueDiffF<F> : ValueDiffFOf<F> {
    // two values that are entirely different
    data class ValueD<F>(val l: KValue, val r: KValue) : ValueDiffF<F>()

    // a value was removed
    data class ValueDRemoved<F>(val v: KValue) : ValueDiffF<F>()

    // a value was added
    data class ValueDAdded<F>(val v: KValue) : ValueDiffF<F>()

    // a tuple that contains at least one diff value that is not Same
    data class TupleD<F>(val vals: List<F>) : ValueDiffF<F>()

    // a list that contains at least one diff value that is not Same
    data class ListD<F>(val vals: List<F>) : ValueDiffF<F>()

    // a record that contains at least one diff value that is not Same
    data class Record<F>(val conName: String, val props: List<Tuple2<String, F>>) : ValueDiffF<F>()

    // a cons, name(prop1, ..., propN), that contains at least one diff value that is not Same
    data class Cons<F>(val consName: String, val props: List<F>) : ValueDiffF<F>()

    // compared values were equal
    data class Same<F>(val v: KValue) : ValueDiffF<F>()

    companion object
}

@extension
interface ValueDiffFFunctor : Functor<ForValueDiffF> {
    override fun <A, B> Kind<ForValueDiffF, A>.map(f: (A) -> B): Kind<ForValueDiffF, B> = when (val d = fix()) {
        is ValueDiffF.ValueD -> ValueDiffF.ValueD(d.l, d.r)
        is ValueDiffF.TupleD -> ValueDiffF.TupleD(d.vals.map(f))
        is ValueDiffF.ListD -> ValueDiffF.ListD(d.vals.map(f))
        is ValueDiffF.Record -> ValueDiffF.Record(d.conName, d.props.map { (k, v) -> k toT f(v) })
        is ValueDiffF.Cons -> ValueDiffF.Cons(d.consName, d.props.map(f))
        is ValueDiffF.Same -> ValueDiffF.Same(d.v)
        is ValueDiffF.ValueDRemoved -> ValueDiffF.ValueDRemoved(d.v)
        is ValueDiffF.ValueDAdded -> ValueDiffF.ValueDAdded(d.v)
    }
}

data class ValueDiff(val unDiff: ValueDiffF<ValueDiff>) {
    companion object
}

@extension
interface ValueDiffBirecursive : Birecursive<ValueDiff, ForValueDiffF> {
    override fun FF(): Functor<ForValueDiffF> = ValueDiffF.functor()
    override fun ValueDiff.projectT(): Kind<ForValueDiffF, ValueDiff> = unDiff
    override fun Kind<ForValueDiffF, ValueDiff>.embedT(): ValueDiff = ValueDiff(this.fix())
}

infix fun KValue.toDiff(other: KValue): ValueDiff = (this toT other).let { (a, b) ->
    when {
        KValue.eq().run { a.eqv(b) } -> ValueDiff(ValueDiffF.Same(a))
        a is KValue.Cons && b is KValue.Cons
                && a.name == b.name ->
            ValueDiff(ValueDiffF.Cons(a.name, a.props.diffOrderedLists(b.props)))
        a is KValue.Record && b is KValue.Record
                && a.name == b.name ->
            ValueDiff(ValueDiffF.Record(a.name, diffMaps(a.kv, b.kv)))
        a is KValue.KTuple && b is KValue.KTuple ->
            ValueDiff(ValueDiffF.TupleD(a.vals.diffOrderedLists(b.vals)))
        a is KValue.KList && b is KValue.KList ->
            ValueDiff(ValueDiffF.ListD(a.vals.diffOrderedLists(b.vals)))
        else -> ValueDiff(ValueDiffF.ValueD(a, b))
    }
}

fun diffMaps(a: List<Tuple2<String, KValue>>, b: List<Tuple2<String, KValue>>): List<Tuple2<String, ValueDiff>> = a.toMap().let { aMap ->
    val diffOrAdd = b.map { (k, v) ->
        if (aMap.containsKey(k)) k toT aMap.getValue(k).toDiff(v)
        else k toT ValueDiff(ValueDiffF.ValueDAdded(v))
    }.toMap()

    val diffOrRemove = a.map { (k, v) ->
        if (diffOrAdd.containsKey(k)) k toT diffOrAdd[k]!!
        else k toT ValueDiff(ValueDiffF.ValueDRemoved(v))
    }

    (diffOrAdd + diffOrRemove).toList().map { (k, v) -> k toT v }
}

// Myer's algorithm
// Based on https://github.com/github/semantic/blob/master/src/Diffing/Algorithm/SES.hs
//  and slightly adapted to kotlin (mainly the array lookup differences)
sealed class Edit {
    data class Remove(val a: KValue) : Edit()
    data class Add(val a: KValue) : Edit()
    data class Compare(val l: KValue, val r: KValue) : Edit()
}

data class Endpoint(val x: Int, val y: Int, val script: List<Edit>)

fun List<KValue>.diffOrderedLists(ls: List<KValue>): List<ValueDiff> {
    val (lArr, rArr) = this.toTypedArray() toT ls.toTypedArray()
    val (m, n) = size toT ls.size

    fun moveDownFrom(e: Endpoint): Endpoint = Endpoint(
        e.x,
        e.y + 1,
        rArr.safeGet(e.y).fold({ e.script }, { listOf(Edit.Add(it)) + e.script })
    )

    fun moveRightFrom(e: Endpoint): Endpoint = Endpoint(
        e.x + 1,
        e.y,
        lArr.safeGet(e.x).fold({ e.script }, { listOf(Edit.Remove(it)) + e.script })
    )

    fun slideFrom(e: Endpoint): Endpoint = Option.fx {
        val l = !lArr.safeGet(e.x)
        val r = !rArr.safeGet(e.y)
        // call shallow diff here (only top level: type and conName)
        if (KValue.eq().run { l.eqv(r) })
            !slideFrom(Endpoint(e.x + 1, e.y + 1, listOf(Edit.Compare(l, r)) + e.script)).some()
        else !none<Endpoint>()
    }.getOrElse { e }

    fun isComplete(e: Endpoint): Boolean = e.x >= m && e.y >= n

    fun searchToD(d: Int, arr: Array<Endpoint>): List<Edit> {
        val offset = d
        fun searchAlongK(k: Int): Endpoint = when (k) {
            -d -> moveDownFrom(arr[k + 1 + offset])
            d -> moveRightFrom(arr[k - 1 + offset])
            -m -> moveDownFrom(arr[k + 1 + offset])
            n -> moveRightFrom(arr[k - 1 + offset])
            else ->
                if (arr[k - 1 + offset].x < arr[k + 1 + offset].x) moveDownFrom(arr[k + 1 + offset])
                else moveRightFrom(arr[k - 1 + offset])
        }

        return (-d..d step 2)
            .filter { it in -m..n }
            .map(::searchAlongK andThen ::slideFrom)
            .let { endpoints ->
                endpoints.firstOrNull(::isComplete).toOption().fold({
                    searchToD(
                        d + 1,
                        endpoints.map { it.x - it.y + d + 1 toT it }.toMap().let { m ->
                            Array(d * 2 + 2) { ind -> m[ind] } as Array<Endpoint>
                        }
                    )
                }, { (_, _, script) -> script })
            }
    }

    val editScript = when {
        lArr.isEmpty() -> rArr.map { Edit.Add(it) }
        rArr.isEmpty() -> lArr.map { Edit.Remove(it) }
        else -> searchToD(0, Array(2) { Endpoint(0, -1, emptyList()) })
    }.reversed()

    return editScript.map {
        when (it) {
            is Edit.Add -> ValueDiff(ValueDiffF.ValueDAdded(it.a))
            is Edit.Remove -> ValueDiff(ValueDiffF.ValueDRemoved(it.a))
            is Edit.Compare -> it.l.toDiff(it.r)
        }
    }
}

fun <T> Array<T>.safeGet(i: Int): Option<T> = when (i) {
    in (0 until size) -> get(i).some()
    else -> None
}

sealed class DiffType {
    object Removed : DiffType() // Identifies a removed element
    object Added : DiffType() // Identifies an added element
}

fun ValueDiff.toLineDiff(): Doc<DiffType> = ValueDiff.birecursive().run {
    KValue.show().run {
        when (val diff = this@toLineDiff.unDiff) {
            // Treat top level value diffs special because the nested diff implementation assumes newlines that this does not have
            is ValueDiffF.ValueD ->
                (("-".text<DiffType>() + space() + diff.l.doc<DiffType>().group())
                    .annotate(DiffType.Removed) +
                        (hardLine<DiffType>() + space() + diff.r.doc<DiffType>().group())
                            .annotate(DiffType.Added))
            else -> cata<Doc<DiffType>> {
                when (val vd = it.fix()) {
                    is ValueDiffF.ValueD -> TODO()
                    // value is the same, use KValue's pretty printer
                    is ValueDiffF.Same -> vd.v.doc()
                    is ValueDiffF.ValueDAdded -> vd.v.doc<DiffType>().group().annotate(DiffType.Added)
                    is ValueDiffF.ValueDRemoved -> vd.v.doc<DiffType>().group().annotate(DiffType.Removed)
                    // everything below contains a diff, that's why custom tuple/list methods are used that are always vertical without group
                    is ValueDiffF.TupleD -> vd.vals.map { it.nest(1) }.tupledNested()
                    is ValueDiffF.ListD -> vd.vals.map { it.nest(1) }.listNested()
                    is ValueDiffF.Cons -> vd.consName.text<DiffType>() +
                            vd.props
                                .map { it.nest(1) }
                                .tupledNested()
                                .nest(vd.consName.length)
                    is ValueDiffF.Record -> vd.conName.text<DiffType>() +
                            vd.props
                                .map { it.a.text<DiffType>() + space() + equals() + space() + it.b.nest(it.a.length + 4) }
                                .map { it.nest(1) }
                                .tupledNested()
                                .nest(vd.conName.length)
                }
            }.indent(1)
        }
    }
}

fun <A> List<Doc<A>>.listNested(): Doc<A> =
    encloseSepVert(
        lBracket<A>() + line() + space() + space(),
        lineBreak<A>() + rBracket(),
        comma<A>() + space()
    )

fun <A> List<Doc<A>>.tupledNested(): Doc<A> =
    encloseSepVert(
        lParen<A>() + line() + space() + space(),
        lineBreak<A>() + rParen(),
        comma<A>() + space()
    )

fun <A> List<Doc<A>>.encloseSepVert(l: Doc<A>, r: Doc<A>, sep: Doc<A>): Doc<A> = when {
    isEmpty() -> l + r
    size == 1 -> l + first() + r
    else -> ((listOf(l toT this.first()) + this.tail().tupleLeft(sep)).map { (a, b) -> a + b }
        .vCat() + r).align()
}

operator fun <A> SimpleDoc<A>.plus(other: SimpleDoc<A>): SimpleDoc<A> = cata {
    when (it) {
        is SimpleDocF.Nil -> other
        else -> SimpleDoc(it)
    }
}

fun SimpleDoc<DiffType>.expandDiff(): SimpleDoc<DiffType> =
    cata<DiffType, (SimpleDoc<DiffType>, SimpleDoc<DiffType>, Boolean) -> SimpleDoc<DiffType>> {
        { prefix, lastLinePre, inDiff ->
            when (val dF = it) {
                is SimpleDocF.Fail -> throw IllegalStateException("Encountered Fail in simple-doc. Please report this.")
                is SimpleDocF.Line ->
                    // add to prefix if we are in a diff, if not add to lastLinePrefix
                    if (inDiff) prefix + dF.doc(SimpleDoc.line(dF.i, SimpleDoc.nil()), lastLinePre, inDiff)
                    else dF.doc(prefix + lastLinePre, SimpleDoc.line(dF.i, SimpleDoc.nil()), inDiff)
                is SimpleDocF.Text ->
                    // if we are in diff, add the text to the prefix, if not add it to lastLinePrefix
                    if (inDiff) dF.doc(prefix + SimpleDoc.text(dF.str, SimpleDoc.nil()), lastLinePre, inDiff)
                    else dF.doc(prefix, lastLinePre + SimpleDoc.text(dF.str, SimpleDoc.nil()), inDiff)
                is SimpleDocF.Nil -> prefix + lastLinePre
                is SimpleDocF.AddAnnotation -> when (dF.ann) {
                    is DiffType.Added -> (if (inDiff) SimpleDoc.nil() else prefix) + SimpleDoc.addAnnotation(
                        dF.ann,
                        dF.doc(SimpleDoc.nil(), lastLinePre, false)
                    )
                    is DiffType.Removed -> (if (inDiff) SimpleDoc.nil() else prefix) + SimpleDoc.addAnnotation(
                        dF.ann,
                        (if (inDiff) prefix else lastLinePre) +
                                dF.doc(SimpleDoc.nil(), lastLinePre, true)
                    )
                }
                is SimpleDocF.RemoveAnnotation -> prefix + (if (inDiff) SimpleDoc.nil() else lastLinePre) + SimpleDoc.removeAnnotation(
                    dF.doc(SimpleDoc.nil(), if (inDiff) lastLinePre else SimpleDoc.nil(), inDiff)
                )
            }
        }
    }.invoke(SimpleDoc.nil(), SimpleDoc.nil(), false)

sealed class LineStatus {
    object Same : LineStatus()
    object Removed : LineStatus()
    object Added : LineStatus()
}

fun SimpleDoc<DiffType>.layoutColored(): String = cata<DiffType, (NonEmptyList<LineStatus>) -> String> {
    { annotations ->
        when (it) {
            is SimpleDocF.Fail -> throw IllegalStateException("Encountered fail in simple doc! Report this please.")
            is SimpleDocF.Nil -> ""
            is SimpleDocF.Text -> it.str + it.doc(annotations)
            is SimpleDocF.Line -> (when (annotations.head) {
                is LineStatus.Same -> "\n" + spaces(it.i)
                is LineStatus.Removed -> "\n-" + spaces(it.i - 1)
                is LineStatus.Added -> "\n+" + spaces(it.i - 1)
            }) + it.doc(annotations)
            is SimpleDocF.AddAnnotation -> when (it.ann) {
                is DiffType.Removed -> "\u001b[31m" + it.doc(Nel(LineStatus.Removed, annotations.all))
                is DiffType.Added -> "\u001b[32m" + it.doc(Nel(LineStatus.Added, annotations.all))
            }
            is SimpleDocF.RemoveAnnotation ->
                "\u001b[0m" + it.doc(Nel.fromListUnsafe(annotations.tail))
        }
    }
}(Nel.of(LineStatus.Same))

// TODO simplify
infix fun String.diff(str: String): String =
    outputParser().runParsecT(State(this, 0)).value().fold({
        TODO()
    }, { a ->
        outputParser().runParsecT(State(str, 0)).value().fold({
            TODO()
        }, { b ->
            (a toDiff b)
                .toLineDiff()
                .renderPretty()
                .expandDiff()
                .layoutColored()
        })
    })
