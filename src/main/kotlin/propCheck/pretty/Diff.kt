package propCheck.pretty

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.fx
import arrow.core.extensions.list.foldable.foldLeft
import arrow.core.extensions.list.functor.map
import arrow.core.extensions.list.functor.tupleLeft
import arrow.core.extensions.list.monadFilter.filterMap
import arrow.extension
import arrow.recursion.typeclasses.Birecursive
import arrow.syntax.collections.tail
import arrow.typeclasses.Functor
import kparsec.runParser
import pretty.*
import pretty.ansistyle.monoid.monoid
import pretty.symbols.*
import propCheck.pretty.kvalue.eq.eq
import propCheck.pretty.valuediff.birecursive.birecursive
import propCheck.pretty.valuedifff.functor.functor
import propCheck.property.Markup
import kotlin.math.min

class ForValueDiffF private constructor()
typealias ValueDiffFOf<F> = Kind<ForValueDiffF, F>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <F> ValueDiffFOf<F>.fix(): ValueDiffF<F> = this as ValueDiffF<F>

sealed class ValueDiffF<out F> : ValueDiffFOf<F> {
    // two values that are entirely different
    data class ValueD(val l: KValue, val r: KValue) : ValueDiffF<Nothing>()

    // a value was removed
    data class ValueDRemoved(val v: KValue) : ValueDiffF<Nothing>()

    // a value was added
    data class ValueDAdded(val v: KValue) : ValueDiffF<Nothing>()

    // a tuple that contains at least one diff value that is not Same
    data class TupleD<F>(val vals: List<F>) : ValueDiffF<F>()

    // a list that contains at least one diff value that is not Same
    data class ListD<F>(val vals: List<F>) : ValueDiffF<F>()

    // a record that contains at least one diff value that is not Same
    data class Record<F>(val conName: String, val props: List<Tuple2<String, F>>) : ValueDiffF<F>()

    // a cons, name(prop1, ..., propN), that contains at least one diff value that is not Same
    data class Cons<F>(val consName: String, val props: List<F>) : ValueDiffF<F>()

    // compared values were equal
    data class Same(val v: KValue) : ValueDiffF<Nothing>()

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

fun diffMaps(a: List<Tuple2<String, KValue>>, b: List<Tuple2<String, KValue>>): List<Tuple2<String, ValueDiff>> =
    a.toMap().let { aMap ->
        val diffOrAdd = b.map { (k, v) ->
            if (aMap.containsKey(k)) k toT aMap.getValue(k).toDiff(v)
            else k toT ValueDiff(ValueDiffF.ValueDAdded(v))
        }.toMap()

        val diffOrRemove = a.filterMap { (k, v) ->
            if (diffOrAdd.containsKey(k)) None
            else (k to ValueDiff(ValueDiffF.ValueDRemoved(v))).some()
        }

        (diffOrAdd.toList() + diffOrRemove).map { (k, v) -> k toT v }
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

    fun Edit.toValueDiff(): ValueDiff = when (this) {
        is Edit.Add -> ValueDiff(ValueDiffF.ValueDAdded(a))
        is Edit.Remove -> ValueDiff(ValueDiffF.ValueDRemoved(a))
        is Edit.Compare -> l.toDiff(r)
    }

    return if (editScript.size >= 2) {
        val fst = editScript[0]
        val snd = editScript[1]
        if (fst is Edit.Remove && snd is Edit.Add) {
            listOf(
                ValueDiff(ValueDiffF.ValueD(fst.a, snd.a))
            ) + editScript.drop(2).map { it.toValueDiff() }
        } else editScript.map { it.toValueDiff() }
    } else editScript.map { it.toValueDiff() }
}

fun <T> Array<T>.safeGet(i: Int): Option<T> = when (i) {
    in (0 until size) -> get(i).some()
    else -> None
}

sealed class DiffType {
    object Removed : DiffType() // Identifies a removed element
    object Added : DiffType() // Identifies an added element
    object Same : DiffType()
}

/**
 * This is quite the hack: In order to get proper prefixes and colors I am abusing some facts about
 *  ValueDiffs. The catamorphism returns an annotation to add to the document + the prefix where the
 *  newline (there has to be one for diffs) is added. This means the annotation includes both the newline
 *  and the doc with the changes. This is later used by the custom renderMarkup method to remove a space
 *  from the SimpleDoc.Line and insert a + or -. Because the resulting diff can be placed at any nested level
 *  itself we must also add a Markup.Diff annotation that has the current column offset around the entire diff.
 *  In the end this all allows diffs to be rendered properly anywhere.
 */
fun ValueDiff.toLineDiff(): Doc<DiffType> = ValueDiff.birecursive().run {
    when (val diff = this@toLineDiff.unDiff) {
        // Treat top level value diffs special because the nested diff implementation assumes newlines that this does not have
        is ValueDiffF.ValueD ->
            // This is the only place where we need to manually add a prefix because this cannot
            //  elevate the annotation to the previous newline
            ("-".text() spaced diff.l.doc()).annotate(DiffType.Removed) +
                    (hardLine() spaced diff.r.doc()).annotate(DiffType.Added)
        is ValueDiffF.Same -> diff.v.doc()
        else ->
            cata<Tuple2<DiffType, Doc<DiffType>>> {
                when (val vd = it.fix()) {
                    is ValueDiffF.ValueD ->
                        DiffType.Removed toT (vd.l.doc() + (hardLine() + vd.r.doc()).annotate(DiffType.Added))
                    // value is the same, use KValue's pretty printer
                    is ValueDiffF.Same -> DiffType.Same toT vd.v.doc()
                    is ValueDiffF.ValueDAdded -> DiffType.Added toT vd.v.doc()
                    is ValueDiffF.ValueDRemoved -> DiffType.Removed toT vd.v.doc()
                    // everything below contains a diff, that's why custom tuple/list methods are used that are always vertical without group
                    is ValueDiffF.TupleD -> DiffType.Same toT vd.vals.tupledNested()
                    is ValueDiffF.ListD -> DiffType.Same toT vd.vals.listNested()
                    is ValueDiffF.Cons -> DiffType.Same toT (vd.consName.text() +
                            vd.props
                                .tupledNested()
                                .align()
                            )
                    is ValueDiffF.Record -> {
                        val max = min(10, vd.props.maxBy { it.a.length }?.a?.length ?: 0)
                        DiffType.Same toT (vd.conName.text() +
                                vd.props
                                    .map { (name, it) ->
                                        val (pre, doc) = it
                                        if (name.length > max)
                                            DiffType.Same toT (name.text() + (lineBreak().nest(max) spaced equals() spaced doc.align()).annotate(
                                                pre
                                            )).align()
                                        else pre toT (name.text().fill(max) spaced equals() spaced doc.align()).align()
                                    }
                                    .tupledNested()
                                    .align())
                    }
                }
            }.b.indent(1) // save space for +/- // This also ensures the layout algorithm is optimal
    }
}

fun <A> List<Tuple2<A, Doc<A>>>.listNested(): Doc<A> =
    encloseSepVert(
        lBracket(),
        hardLine() + rBracket(),
        comma() + space()
    )

fun <A> List<Tuple2<A, Doc<A>>>.tupledNested(): Doc<A> =
    encloseSepVert(
        lParen(),
        hardLine() + rParen(),
        comma() + space()
    )

fun <A> List<Tuple2<A, Doc<A>>>.encloseSepVert(l: Doc<A>, r: Doc<A>, sep: Doc<A>): Doc<A> = when {
    isEmpty() -> l + r
    size == 1 -> l + first().let { (t, doc) -> (hardLine() + doc).annotate(t) }.align() + r
    else -> l + ((listOf((hardLine() + space() + space()) toT this.first()) + this.tail().tupleLeft(sep))
        .map { (a, b) -> b.a toT (a + b.b.align()) }
        .let { xs ->
            if (xs.size == 1) xs.first().let { (ann, d) -> d.annotate(ann) }
            else xs.tail().foldLeft(xs.first().let { (ann, d) -> d.annotate(ann) }) { acc, (ann, d) ->
                (acc + (hardLine() + d).annotate(ann))
            }
        } + r).align()
}

infix fun String.diff(str: String): ValueDiff {
    val lhs = outputParser().runParser("", this).fold({
        KValue.RawString(this)
    }, ::identity)
    val rhs = outputParser().runParser("", str).fold({
        KValue.RawString(str)
    }, ::identity)

    return lhs.toDiff(rhs)
}

fun ValueDiff.toDoc(): Doc<Markup> =
    column { cc ->
        toLineDiff().alterAnnotations {
            when (it) {
                is DiffType.Removed -> listOf(Markup.DiffRemoved(cc))
                is DiffType.Added -> listOf(Markup.DiffAdded(cc))
                is DiffType.Same -> emptyList()
            }
        }
    }.annotate(Markup.Diff)
