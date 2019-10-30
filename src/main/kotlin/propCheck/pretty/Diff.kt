package propCheck.pretty

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.id.monad.monad
import arrow.core.extensions.list.functor.tupleLeft
import arrow.extension
import arrow.recursion.typeclasses.Birecursive
import arrow.syntax.collections.tail
import arrow.typeclasses.Functor
import pretty.*
import pretty.simpledoc.birecursive.birecursive
import propCheck.pretty.kvalue.eq.eq
import propCheck.pretty.kvalue.show.show
import propCheck.pretty.parse.runParsecT
import propCheck.pretty.valuediff.birecursive.birecursive
import propCheck.pretty.valuedifff.functor.functor

class ForValueDiffF private constructor()
typealias ValueDiffFOf<F> = Kind<ForValueDiffF, F>

@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <F> ValueDiffFOf<F>.fix(): ValueDiffF<F> = this as ValueDiffF<F>

sealed class ValueDiffF<F> : ValueDiffFOf<F> {
    data class ValueD<F>(val l: KValue, val r: KValue) : ValueDiffF<F>()
    data class TupleD<F>(val vals: List<F>) : ValueDiffF<F>()
    data class ListD<F>(val vals: List<F>) : ValueDiffF<F>()
    // arrow sometimes goes either cons or record while kotlin usually does record
    data class Record<F>(val conName: String, val props: List<Tuple2<String, F>>) : ValueDiffF<F>()

    data class Cons<F>(val consName: String, val props: List<F>) : ValueDiffF<F>()
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

infix fun KValue.toDiff(other: KValue): ValueDiff = ValueDiff.birecursive().run {
    KValue.eq().run {
        (this@toDiff to other).ana { (a, b) ->
            when {
                a.eqv(b) -> ValueDiffF.Same(a)
                a is KValue.Cons && b is KValue.Cons
                        && a.name == b.name
                        && a.props.size == b.props.size ->
                    ValueDiffF.Cons(a.name, a.props.zip(b.props))
                a is KValue.Record && b is KValue.Record
                        && a.name == b.name
                        && a.kv.map { it.a } == b.kv.map { it.a } ->
                    ValueDiffF.Record(
                        a.name,
                        a.kv.zip(b.kv) { f, s ->
                            f.a toT (f.b to s.b)
                        }
                    )
                a is KValue.KTuple && b is KValue.KTuple
                        && a.vals.size == b.vals.size ->
                    ValueDiffF.TupleD(a.vals.zip(b.vals))
                a is KValue.KList && b is KValue.KList
                        && a.vals.size == b.vals.size ->
                    ValueDiffF.ListD(a.vals.zip(b.vals))
                else -> ValueDiffF.ValueD(a, b)
            }
        }
    }
}

sealed class DiffType {
    object Removed : DiffType() // Identifies a removed element
    object Added : DiffType() // Identifies an added element
    object Diff : DiffType() // Identifies a diff region
}

// Add a top down fold that accumulates prefixes for annotated parts (over simpledoc)
// Then do two things: - Expand Removed diffs to surround this prefix
//                     - Add the prefix to Added diffs
// Then later on when rendering to string replace calls to Line that are wrapped in annotate by
//   a special implementation that adds - or +

fun ValueDiff.toLineDiff(): Doc<DiffType> = ValueDiff.birecursive().run {
    KValue.show().run {
        when (val diff = this@toLineDiff.unDiff) {
            // Treat top level value diffs special
            is ValueDiffF.ValueD ->
                (("-".text<DiffType>() + space() + diff.l.doc<DiffType>().flatten())
                    .annotate(DiffType.Removed) lineBreak
                        ("+".text<DiffType>() + space() + diff.r.doc<DiffType>().flatten())
                            .annotate(DiffType.Added)).annotate(DiffType.Diff)
            else -> cata<Doc<DiffType>> {
                when (val vd = it.fix()) {
                    is ValueDiffF.Same -> vd.v.doc() // can contain newlines hence doc
                    is ValueDiffF.ValueD ->
                        ((vd.l.doc<DiffType>().flatten()).annotate(DiffType.Removed) +
                                (vd.r.doc<DiffType>().flatten()).annotate(DiffType.Added)).annotate(DiffType.Diff)
                            .annotate(DiffType.Diff)
                    is ValueDiffF.TupleD -> vd.vals.map { it.nest(1) }.tupledNested()
                    is ValueDiffF.ListD -> vd.vals.map { it.nest(1) }.listNested()
                    is ValueDiffF.Cons -> vd.consName.text<DiffType>() +
                            vd.props.tupledNested().nest(vd.consName.length + 1)
                    is ValueDiffF.Record -> vd.conName.text<DiffType>() +
                            vd.props
                                .map { it.a.text<DiffType>() + space() + equals() + space() + it.b.nest(it.a.length + 4) }
                                .tupledNested()
                                .nest(vd.conName.length + 1)
                }
            }
        }
    }
}

fun <A> List<Doc<A>>.listNested(): Doc<A> =
    encloseSepVert(lBracket<A>() + lineBreak() + space(), lineBreak<A>() + rBracket(), comma())

fun <A> List<Doc<A>>.tupledNested(): Doc<A> =
    encloseSepVert(lParen<A>() + lineBreak() + space() + space(), lineBreak<A>() + rParen(), comma<A>() + space())

fun <A> List<Doc<A>>.encloseSepVert(l: Doc<A>, r: Doc<A>, sep: Doc<A>): Doc<A> = when {
    isEmpty() -> l + r
    size == 1 -> l + first() + r
    else -> ((listOf(l toT this.first()) + this.tail().tupleLeft(sep)).map { (a, b) -> a + b }
        .vCat() + r).align()
}

fun SimpleDoc<DiffType>.expandDiff(): SimpleDoc<DiffType> = SimpleDoc.birecursive<DiffType>().run {
    this@expandDiff.cata<(SimpleDoc<DiffType>, Int) -> SimpleDoc<DiffType>> {
        { prefix, inDiff ->
            when (val dF = it.fix()) {
                is SimpleDocF.Line ->
                    if (inDiff > 0) SimpleDoc.line(dF.i, dF.doc(prefix, inDiff))
                    else prefix + dF.doc(SimpleDoc.line(dF.i, SimpleDoc.nil()), inDiff)
                is SimpleDocF.Text ->
                    if (inDiff > 0) SimpleDoc.text(dF.str, dF.doc(prefix, inDiff))
                    else dF.doc(prefix + SimpleDoc.text(dF.str, SimpleDoc.nil()), inDiff)
                is SimpleDocF.NilF -> prefix
                is SimpleDocF.AddAnnotation -> when (dF.ann) {
                    is DiffType.Diff -> SimpleDoc.addAnnotation(
                        dF.ann,
                        dF.doc(prefix, 1)
                    )
                    is DiffType.Added -> SimpleDoc.addAnnotation(
                        dF.ann,
                        (if (inDiff > 0) prefix else SimpleDoc.nil()) +
                                dF.doc(prefix, inDiff + 1)
                    )
                    is DiffType.Removed -> SimpleDoc.addAnnotation(
                        dF.ann,
                        (if (inDiff > 0) prefix else SimpleDoc.nil()) +
                                dF.doc(prefix, inDiff + 1)
                    )
                }
                is SimpleDocF.RemoveAnnotation -> SimpleDoc.removeAnnotation(
                    dF.doc(if (inDiff - 1 > 0) prefix else SimpleDoc.nil(), inDiff - 1)
                )
            }
        }
    }.invoke(SimpleDoc.nil(), 0)
}

sealed class LineStatus {
    object Same : LineStatus()
    object Removed : LineStatus()
    object Added : LineStatus()
}

fun SimpleDoc<DiffType>.layoutColored(): String = SimpleDoc.birecursive<DiffType>().run {
    cata<(NonEmptyList<LineStatus>) -> String> {
        { ls ->
            ls.head.let { status ->
                when (val dF = it.fix()) {
                    is SimpleDocF.NilF -> ""
                    is SimpleDocF.Text -> dF.str + dF.doc(ls)
                    is SimpleDocF.Line -> (when (status) {
                        is LineStatus.Same -> "\n" + spaces(dF.i)
                        is LineStatus.Removed -> "\n-" + spaces(dF.i - 1)
                        is LineStatus.Added -> "\n+" + spaces(dF.i - 1)
                    }) + dF.doc(ls)
                    is SimpleDocF.AddAnnotation -> when (dF.ann) {
                        is DiffType.Removed -> "\u001b[31m" + dF.doc(Nel(LineStatus.Removed, ls.all))
                        is DiffType.Added -> "\u001b[32m" + dF.doc(Nel(LineStatus.Added, ls.all))
                        is DiffType.Diff -> dF.doc(Nel(LineStatus.Same, ls.all))
                    }
                    // every annotation adds one, so after every remove we will still have one
                    is SimpleDocF.RemoveAnnotation -> "\u001b[0m" + dF.doc(Nel.fromListUnsafe(ls.tail))
                }
            }
        }
    }(Nel.of(LineStatus.Same))
}

// TODO simplify
infix fun String.diff(str: String): String =
    outputParser().runParsecT(this, Id.monad()).value().fold({
        TODO()
    }, { a ->
        outputParser().runParsecT(str, Id.monad()).value().fold({
            TODO()
        }, { b ->
            (a toDiff b)
                .toLineDiff()
                .renderPretty()
                .expandDiff()
                .layoutColored()
        })
    })
