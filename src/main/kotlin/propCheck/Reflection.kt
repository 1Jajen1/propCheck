package propCheck

import arrow.core.*
import arrow.core.extensions.either.show.show
import arrow.core.extensions.id.show.show
import arrow.core.extensions.option.show.show
import arrow.core.extensions.show
import arrow.data.*
import arrow.data.extensions.ior.show.show
import arrow.data.extensions.mapk.show.show
import arrow.data.extensions.nonemptylist.show.show
import arrow.data.extensions.setk.show.show
import arrow.data.extensions.validated.show.show
import arrow.effects.IO
import arrow.effects.extensions.io.applicativeError.handleError
import arrow.effects.extensions.io.monadThrow.monadThrow
import arrow.typeclasses.Show
import propCheck.asciistring.arbitrary.arbitrary
import propCheck.asciistring.show.show
import propCheck.blind.arbitrary.arbitrary
import propCheck.blind.show.show
import propCheck.fixed.arbitrary.arbitrary
import propCheck.fixed.show.show
import propCheck.instances.*
import propCheck.instances.either.arbitrary.arbitrary
import propCheck.instances.id.arbitrary.arbitrary
import propCheck.instances.ior.arbitrary.arbitrary
import propCheck.instances.listk.arbitrary.arbitrary
import propCheck.instances.mapk.arbitrary.arbitrary
import propCheck.instances.nonemptylist.arbitrary.arbitrary
import propCheck.instances.option.arbitrary.arbitrary
import propCheck.instances.setk.arbitrary.arbitrary
import propCheck.instances.tuple10.arbitrary.arbitrary
import propCheck.instances.tuple11.arbitrary.arbitrary
import propCheck.instances.tuple12.arbitrary.arbitrary
import propCheck.instances.tuple13.arbitrary.arbitrary
import propCheck.instances.tuple14.arbitrary.arbitrary
import propCheck.instances.tuple15.arbitrary.arbitrary
import propCheck.instances.tuple16.arbitrary.arbitrary
import propCheck.instances.tuple17.arbitrary.arbitrary
import propCheck.instances.tuple18.arbitrary.arbitrary
import propCheck.instances.tuple19.arbitrary.arbitrary
import propCheck.instances.tuple2.arbitrary.arbitrary
import propCheck.instances.tuple20.arbitrary.arbitrary
import propCheck.instances.tuple21.arbitrary.arbitrary
import propCheck.instances.tuple22.arbitrary.arbitrary
import propCheck.instances.tuple3.arbitrary.arbitrary
import propCheck.instances.tuple4.arbitrary.arbitrary
import propCheck.instances.tuple5.arbitrary.arbitrary
import propCheck.instances.tuple6.arbitrary.arbitrary
import propCheck.instances.tuple7.arbitrary.arbitrary
import propCheck.instances.tuple8.arbitrary.arbitrary
import propCheck.instances.tuple9.arbitrary.arbitrary
import propCheck.instances.validated.arbitrary.arbitrary
import propCheck.negative.arbitrary.arbitrary
import propCheck.negative.show.show
import propCheck.nonnegative.arbitrary.arbitrary
import propCheck.nonnegative.show.show
import propCheck.nonpositive.arbitrary.arbitrary
import propCheck.nonpositive.show.show
import propCheck.positive.arbitrary.arbitrary
import propCheck.positive.show.show
import propCheck.shrink2.arbitrary.arbitrary
import propCheck.shrink2.show.show
import propCheck.smart.arbitrary.arbitrary
import propCheck.smart.show.show
import propCheck.unicodestring.arbitrary.arbitrary
import propCheck.unicodestring.show.show
import java.lang.reflect.ParameterizedType
import java.lang.reflect.Type
import java.lang.reflect.WildcardType

/**
 * This file is super ugly codewise, it does however work and can recursively lookup instances even with generics
 * This makes having to define (at least the standart ones) arbitrary instances unnecessary
 */

// ------------------ Show
fun <A : Any> lookupShow(qualifiedName: String): Show<A> = when (qualifiedName) {
    "kotlin.Int", "java.lang.Integer" -> Int.show()
    "kotlin.Long", "java.lang.Long" -> Long.show()
    "kotlin.String", "java.lang.String" -> String.show()
    "kotlin.Char", "java.lang.Char" -> Char.show()
    "kotlin.Float", "java.lang.Float" -> Float.show()
    "kotlin.Double", "java.lang.Double" -> Double.show()
    "kotlin.Boolean", "java.lang.Boolean" -> Boolean.show()
    "kotlin.Byte", "java.lang.Byte" -> Byte.show()
    IntArray::class.qualifiedName, IntArray::class.java.name -> Show<IntArray> {
        "[" + toList().joinToString {
            Int.show().run { it.show() }
        } + "]"
    }
    LongArray::class.qualifiedName, LongArray::class.java.name -> Show<LongArray> {
        "[" + toList().joinToString {
            Long.show().run { it.show() }
        } + "]"
    }
    FloatArray::class.qualifiedName, FloatArray::class.java.name -> Show<FloatArray> {
        "[" + toList().joinToString {
            Float.show().run { it.show() }
        } + "]"
    }
    DoubleArray::class.qualifiedName, DoubleArray::class.java.name -> Show<DoubleArray> {
        "[" + toList().joinToString {
            Double.show().run { it.show() }
        } + "]"
    }
    ByteArray::class.qualifiedName, ByteArray::class.java.name -> Show<ByteArray> {
        "[" + toList().joinToString {
            Byte.show().run { it.show() }
        } + "]"
    }
    BooleanArray::class.qualifiedName, BooleanArray::class.java.name -> Show<BooleanArray> {
        "[" + toList().joinToString {
            Boolean.show().run { it.show() }
        } + "]"
    }
    ASCIIString::class.qualifiedName, ASCIIString::class.java.name -> ASCIIString.show()
    UnicodeString::class.qualifiedName, UnicodeString::class.java.name -> UnicodeString.show()
    else -> Show.any()
} as Show<A>

inline fun <reified A : Any> defShow(): Show<A> = IO.monadThrow().bindingCatch {
    Nel.fromList(getGenericTypes<A>()).fold({
        lookupShow<A>(A::class.qualifiedName!!)
    }, {
        lookupShowWithGenerics(A::class.java, it)
    })
}.handleError { Show.any() }.unsafeRunSync()

fun <A : Any> lookupShowWithGenerics(klass: Class<A>, l: Nel<Type>): Show<A> =
    klass.name.let { name ->
        when { // I am keeping the KClass qualified names for now because i might want to use them later ...
            name.startsWith("arrow.core.Tuple") -> Show.any()
            name == Pair::class.qualifiedName || name == Pair::class.java.name -> lookupShowNParameters(klass, 2, l) {
                object : PairShow<Any, Any> {
                    override fun SA(): Show<Any> = it[0] as Show<Any>
                    override fun SB(): Show<Any> = it[1] as Show<Any>
                }
            }
            name == Triple::class.qualifiedName || name == Triple::class.java.name -> lookupShowNParameters(
                klass,
                2,
                l
            ) {
                object : TripleShow<Any, Any, Any> {
                    override fun SA(): Show<Any> = it[0] as Show<Any>
                    override fun SB(): Show<Any> = it[1] as Show<Any>
                    override fun SC(): Show<Any> = it[2] as Show<Any>
                }
            }
            name == List::class.qualifiedName || name == ListK::class.qualifiedName ||
                    name == List::class.java.name || name == ListK::class.java.name -> lookupShowNParameters(
                klass,
                1,
                l
            ) {
                object : ListKShow<Any> {
                    override fun SA(): Show<Any> = it[0] as Show<Any>
                }
            }
            name == Set::class.qualifiedName || name == SetK::class.qualifiedName ||
                    name == Set::class.java.name || name == SetK::class.java.name -> lookupShowNParameters(klass, 1, l) {
                object : SetKShow<Any> {
                    override fun SA(): Show<Any> = it[0] as Show<Any>
                }
            }
            name == Map::class.qualifiedName || name == MapK::class.qualifiedName ||
                    name == Map::class.java.name || name == MapK::class.java.name -> lookupShowNParameters(klass, 2, l) {
                object: MapKShow<Any, Any> {
                    override fun SK(): Show<Any> = it[0] as Show<Any>
                    override fun SV(): Show<Any> = it[1] as Show<Any>
                }
            }
            name == Blind::class.qualifiedName || name == Blind::class.java.name -> Blind.show<Any>()
            name == Fixed::class.qualifiedName || name == Fixed::class.java.name -> lookupShowNParameters(
                klass,
                1,
                l
            ) { Fixed.show(it[0]) }
            name == Smart::class.qualifiedName || name == Smart::class.java.name -> lookupShowNParameters(
                klass,
                1,
                l
            ) { Smart.show(it[0]) }
            name == Shrink2::class.qualifiedName || name == Shrink2::class.java.name -> lookupShowNParameters(
                klass,
                1,
                l
            ) { Shrink2.show(it[0]) }
            name == Either::class.qualifiedName || name == Either::class.java.name -> lookupShowNParameters(klass, 2, l) {
                object: EitherShow<Any, Any> {
                    override fun SL(): Show<Any> = it[0] as Show<Any>
                    override fun SR(): Show<Any> = it[1] as Show<Any>
                }
            }
            name == Option::class.qualifiedName || name == Option::class.java.name -> lookupShowNParameters(klass, 1, l) {
                object: OptionShow<Any> {
                    override fun SA(): Show<Any> = it[0] as Show<Any>
                }
            }
            name == Id::class.qualifiedName || name == Id::class.java.name -> lookupShowNParameters(klass, 1, l) {
                object: IdShow<Any> {
                    override fun SA(): Show<Any> = it[0] as Show<Any>
                }
            }
            name == Ior::class.qualifiedName || name == Ior::class.java.name -> lookupShowNParameters(klass, 2, l) {
                object: IorShow<Any, Any> {
                    override fun SL(): Show<Any> = it[0] as Show<Any>
                    override fun SR(): Show<Any> = it[1] as Show<Any>
                }
            }
            name == NonEmptyList::class.qualifiedName || name == NonEmptyList::class.java.name -> lookupShowNParameters(klass, 1, l) {
                object: NonEmptyListShow<Any> {
                    override fun SA(): Show<Any> = it[0] as Show<Any>
                }
            }
            name == Validated::class.qualifiedName || name == Validated::class.java.name -> lookupShowNParameters(klass, 2, l) {
                object: ValidatedShow<Any, Any> {
                    override fun SE(): Show<Any> = it[0] as Show<Any>
                    override fun SA(): Show<Any> = it[1] as Show<Any>
                }
            }
            name == Positive::class.qualifiedName || name == Positive::class.java.name -> lookupShowNParameters(
                klass,
                1,
                l
            ) { Positive.show(it[0]) }
            name == NonNegative::class.qualifiedName || name == NonNegative::class.java.name -> lookupShowNParameters(
                klass,
                1,
                l
            ) { NonNegative.show(it[0]) }
            name == Negative::class.qualifiedName || name == Negative::class.java.name -> lookupShowNParameters(
                klass,
                1,
                l
            ) { Negative.show(it[0]) }
            name == NonPositive::class.qualifiedName || name == NonPositive::class.java.name -> lookupShowNParameters(
                klass,
                1,
                l
            ) { NonPositive.show(it[0]) }
            else -> Show.any()
        } as Show<A>
    }

fun lookupShowWithPossibleGenerics(type: Type): Show<*> = when (type) {
    is ParameterizedType -> Nel.fromList(type.actualTypeArguments.toList()).fold({
        lookupShowByName(className(0, Nel.of(type)))
    }, {
        lookupShowWithGenerics(type.rawType as Class<*>, it)
    })
    is WildcardType -> lookupShowWithPossibleGenerics(type.upperBounds.first())
    is Class<*> -> lookupShowByName(type.name)
    else -> println("WTF ARE YOU $type").let { throw IllegalStateException("WTF") }
}

fun <A : Any> lookupShowNParameters(klass: Class<A>, n: Int, l: Nel<Type>, cf: (List<Show<*>>) -> Show<*>): Show<*> =
    if (l.size != n) Show.any()
    else cf((1..n).map { lookupShowWithPossibleGenerics(l.all[it - 1]) })

fun lookupShowByName(s: String): Show<*> = lookupShow<Any>(s)

// ------------------ Arbitrary

fun <A : Any> lookupArby(qualifiedName: String): Arbitrary<A> = when (qualifiedName) {
    "kotlin.Int", "java.lang.Integer" -> Int.arbitrary()
    "kotlin.Long", "java.lang.Long" -> Long.arbitrary()
    "kotlin.String", "java.lang.String" -> String.arbitrary()
    "kotlin.Char", "java.lang.Char" -> Char.arbitrary()
    "kotlin.Float", "java.lang.Float" -> Float.arbitrary()
    "kotlin.Double", "java.lang.Double" -> Double.arbitrary()
    "kotlin.Boolean", "java.lang.Boolean" -> Boolean.arbitrary()
    "kotlin.Byte", "java.lang.Byte" -> Byte.arbitrary()
    IntArray::class.qualifiedName, IntArray::class.java.name -> intArrayArb
    LongArray::class.qualifiedName, LongArray::class.java.name -> longArrayArb
    FloatArray::class.qualifiedName, FloatArray::class.java.name -> floatArrayArb
    DoubleArray::class.qualifiedName, DoubleArray::class.java.name -> doubleArrayArb
    ByteArray::class.qualifiedName, ByteArray::class.java.name -> byteArrayArb
    BooleanArray::class.qualifiedName, BooleanArray::class.java.name -> booleanArrayArb
    ASCIIString::class.qualifiedName, ASCIIString::class.java.name -> ASCIIString.arbitrary()
    UnicodeString::class.qualifiedName, UnicodeString::class.java.name -> UnicodeString.arbitrary()
    else -> throw IllegalStateException("Could not find default arbitrary for $qualifiedName")
} as Arbitrary<A>

inline fun <reified A : Any> getGenericTypes(): List<Type> = object : TypeReference<A>() {}.type.let {
    if (it is ParameterizedType)
        it.actualTypeArguments
    else emptyArray<Type>()
}.toList()

abstract class TypeReference<T> : Comparable<TypeReference<T>> {
    val type: Type = (javaClass.genericSuperclass as ParameterizedType).actualTypeArguments[0]
    override fun compareTo(other: TypeReference<T>) = 0
}

inline fun <reified A : Any> defArbitrary(): Arbitrary<A> = Nel.fromList(getGenericTypes<A>()).fold({
    lookupArby(A::class.qualifiedName!!)
}, {
    lookupArbyWithGenerics(A::class.java, it)
})

fun lookupArbyWithPossibleGenerics(type: Type): Arbitrary<*> = when (type) {
    is ParameterizedType -> Nel.fromList(type.actualTypeArguments.toList()).fold({
        lookupArbyByName(className(0, Nel.of(type)))
    }, {
        lookupArbyWithGenerics(type.rawType as Class<*>, it)
    })
    is WildcardType -> lookupArbyWithPossibleGenerics(type.upperBounds.first())
    is Class<*> -> lookupArbyByName(type.name)
    else -> println("WTF ARE YOU $type").let { throw IllegalStateException("WTF") }
}

fun <A : Any> lookupArbitraryNParameters(
    klass: Class<A>,
    n: Int,
    l: Nel<Type>,
    cf: (List<Arbitrary<*>>) -> Arbitrary<*>
): Arbitrary<*> =
    if (l.size != n) throw IllegalStateException("Could not find default arbitrary for ${klass.name}")
    else cf((1..n).map { lookupArbyWithPossibleGenerics(l.all[it - 1]) })

fun <A : Any> lookupArbyWithGenerics(klass: Class<A>, l: Nel<Type>): Arbitrary<A> =
    klass.name.let { name ->
        when { // I am keeping the KClass qualified names for now because i might want to use them later ...
            name.startsWith("arrow.core.Tuple") -> lookupTuple(klass, Integer.parseInt("${name[16]}"), l)
            name == Pair::class.qualifiedName || name == Pair::class.java.name -> lookupArbitraryNParameters(
                klass,
                2,
                l
            ) {
                object : PairArbitrary<Any, Any> {
                    override fun AA(): Arbitrary<Any> = it[0] as Arbitrary<Any>
                    override fun AB(): Arbitrary<Any> = it[1] as Arbitrary<Any>
                }
            }
            name == Triple::class.qualifiedName || name == Triple::class.java.name -> lookupArbitraryNParameters(
                klass,
                3,
                l
            ) {
                object : TripleArbitrary<Any, Any, Any> {
                    override fun AA(): Arbitrary<Any> = it[0] as Arbitrary<Any>
                    override fun AB(): Arbitrary<Any> = it[1] as Arbitrary<Any>
                    override fun AC(): Arbitrary<Any> = it[2] as Arbitrary<Any>
                }
            }
            name == List::class.qualifiedName || name == ListK::class.qualifiedName ||
                    name == List::class.java.name || name == ListK::class.java.name -> lookupArbitraryNParameters(
                klass,
                1,
                l
            ) { ListK.arbitrary(it[0]) }
            name == Set::class.qualifiedName || name == SetK::class.qualifiedName ||
                    name == Set::class.java.name || name == SetK::class.java.name -> lookupArbitraryNParameters(
                klass,
                1,
                l
            ) { SetK.arbitrary(it[0]) }
            name == Map::class.qualifiedName || name == MapK::class.qualifiedName ||
                    name == Map::class.java.name || name == MapK::class.java.name -> lookupArbitraryNParameters(
                klass,
                2,
                l
            ) { MapK.arbitrary(it[0], it[1]) }
            name == Blind::class.qualifiedName || name == Blind::class.java.name -> lookupArbitraryNParameters(
                klass,
                1,
                l
            ) { Blind.arbitrary(it[0]) }
            name == Fixed::class.qualifiedName || name == Fixed::class.java.name -> lookupArbitraryNParameters(
                klass,
                1,
                l
            ) { Fixed.arbitrary(it[0]) }
            name == Smart::class.qualifiedName || name == Smart::class.java.name -> lookupArbitraryNParameters(
                klass,
                1,
                l
            ) { Smart.arbitrary(it[0]) }
            name == Shrink2::class.qualifiedName || name == Shrink2::class.java.name -> lookupArbitraryNParameters(
                klass,
                1,
                l
            ) { Shrink2.arbitrary(it[0]) }
            name == Either::class.qualifiedName || name == Either::class.java.name -> lookupArbitraryNParameters(
                klass,
                2,
                l
            ) { Either.arbitrary(it[0], it[1]) }
            name == Option::class.qualifiedName || name == Option::class.java.name -> lookupArbitraryNParameters(
                klass,
                1,
                l
            ) { Option.arbitrary(it[0]) }
            name == Id::class.qualifiedName || name == Id::class.java.name -> lookupArbitraryNParameters(
                klass,
                1,
                l
            ) { Id.arbitrary(it[0]) }
            name == Ior::class.qualifiedName || name == Ior::class.java.name -> lookupArbitraryNParameters(
                klass,
                2,
                l
            ) { Ior.arbitrary(it[0], it[1]) }
            name == NonEmptyList::class.qualifiedName || name == NonEmptyList::class.java.name -> lookupArbitraryNParameters(
                klass,
                1,
                l
            ) { Nel.arbitrary(it[0]) }
            name == Validated::class.qualifiedName || name == Validated::class.java.name -> lookupArbitraryNParameters(
                klass,
                2,
                l
            ) { Validated.arbitrary(it[0], it[1]) }
            name == Positive::class.qualifiedName || name == Positive::class.java.name -> lookupArbitraryNParameters(
                klass,
                1,
                l
            ) { Positive.arbitrary(it[0] as Arbitrary<Number>) }
            name == NonNegative::class.qualifiedName || name == NonNegative::class.java.name -> lookupArbitraryNParameters(
                klass,
                1,
                l
            ) { NonNegative.arbitrary(it[0] as Arbitrary<Number>) }
            name == Negative::class.qualifiedName || name == Negative::class.java.name -> lookupArbitraryNParameters(
                klass,
                1,
                l
            ) { Negative.arbitrary(it[0] as Arbitrary<Number>) }
            name == NonPositive::class.qualifiedName || name == NonPositive::class.java.name -> lookupArbitraryNParameters(
                klass,
                1,
                l
            ) { NonPositive.arbitrary(it[0] as Arbitrary<Number>) }
            else -> throw IllegalStateException("Unsupported class $name")
        } as Arbitrary<A>
    }

fun <A : Any> lookupTuple(klass: Class<A>, n: Int, l: Nel<Type>): Arbitrary<A> =
    if (l.size != n) throw IllegalStateException("Could not find default arbitrary for ${klass.name}")
    else when (n) {
        2 -> Tuple2.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1])
        )
        3 -> Tuple3.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2])
        )
        4 -> Tuple4.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3])
        )
        5 -> Tuple5.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4])
        )
        6 -> Tuple6.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4]),
            lookupArbyWithPossibleGenerics(l.all[5])
        )
        7 -> Tuple7.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4]),
            lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[5])
        )
        8 -> Tuple8.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4]),
            lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6])
        )
        9 -> Tuple9.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4]),
            lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]),
            lookupArbyWithPossibleGenerics(l.all[7]),
            lookupArbyWithPossibleGenerics(l.all[8])
        )
        10 -> Tuple10.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4]),
            lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]),
            lookupArbyWithPossibleGenerics(l.all[7]),
            lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9])
        )
        11 -> Tuple11.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4]),
            lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]),
            lookupArbyWithPossibleGenerics(l.all[7]),
            lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]),
            lookupArbyWithPossibleGenerics(l.all[10])
        )
        12 -> Tuple12.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4]),
            lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]),
            lookupArbyWithPossibleGenerics(l.all[7]),
            lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]),
            lookupArbyWithPossibleGenerics(l.all[10]),
            lookupArbyWithPossibleGenerics(l.all[11])
        )
        13 -> Tuple13.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4]),
            lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]),
            lookupArbyWithPossibleGenerics(l.all[7]),
            lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]),
            lookupArbyWithPossibleGenerics(l.all[10]),
            lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12])
        )
        14 -> Tuple14.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4]),
            lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]),
            lookupArbyWithPossibleGenerics(l.all[7]),
            lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]),
            lookupArbyWithPossibleGenerics(l.all[10]),
            lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]),
            lookupArbyWithPossibleGenerics(l.all[13])
        )
        15 -> Tuple15.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4]),
            lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]),
            lookupArbyWithPossibleGenerics(l.all[7]),
            lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]),
            lookupArbyWithPossibleGenerics(l.all[10]),
            lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]),
            lookupArbyWithPossibleGenerics(l.all[13]),
            lookupArbyWithPossibleGenerics(l.all[14])
        )
        16 -> Tuple16.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4]),
            lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]),
            lookupArbyWithPossibleGenerics(l.all[7]),
            lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]),
            lookupArbyWithPossibleGenerics(l.all[10]),
            lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]),
            lookupArbyWithPossibleGenerics(l.all[13]),
            lookupArbyWithPossibleGenerics(l.all[14]),
            lookupArbyWithPossibleGenerics(l.all[15])
        )
        17 -> Tuple17.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4]),
            lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]),
            lookupArbyWithPossibleGenerics(l.all[7]),
            lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]),
            lookupArbyWithPossibleGenerics(l.all[10]),
            lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]),
            lookupArbyWithPossibleGenerics(l.all[13]),
            lookupArbyWithPossibleGenerics(l.all[14]),
            lookupArbyWithPossibleGenerics(l.all[15]),
            lookupArbyWithPossibleGenerics(l.all[16])
        )
        18 -> Tuple18.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4]),
            lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]),
            lookupArbyWithPossibleGenerics(l.all[7]),
            lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]),
            lookupArbyWithPossibleGenerics(l.all[10]),
            lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]),
            lookupArbyWithPossibleGenerics(l.all[13]),
            lookupArbyWithPossibleGenerics(l.all[14]),
            lookupArbyWithPossibleGenerics(l.all[15]),
            lookupArbyWithPossibleGenerics(l.all[16]),
            lookupArbyWithPossibleGenerics(l.all[17])
        )
        19 -> Tuple19.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4]),
            lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]),
            lookupArbyWithPossibleGenerics(l.all[7]),
            lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]),
            lookupArbyWithPossibleGenerics(l.all[10]),
            lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]),
            lookupArbyWithPossibleGenerics(l.all[13]),
            lookupArbyWithPossibleGenerics(l.all[14]),
            lookupArbyWithPossibleGenerics(l.all[15]),
            lookupArbyWithPossibleGenerics(l.all[16]),
            lookupArbyWithPossibleGenerics(l.all[17]),
            lookupArbyWithPossibleGenerics(l.all[18])
        )
        20 -> Tuple20.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4]),
            lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]),
            lookupArbyWithPossibleGenerics(l.all[7]),
            lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]),
            lookupArbyWithPossibleGenerics(l.all[10]),
            lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]),
            lookupArbyWithPossibleGenerics(l.all[13]),
            lookupArbyWithPossibleGenerics(l.all[14]),
            lookupArbyWithPossibleGenerics(l.all[15]),
            lookupArbyWithPossibleGenerics(l.all[16]),
            lookupArbyWithPossibleGenerics(l.all[17]),
            lookupArbyWithPossibleGenerics(l.all[18]),
            lookupArbyWithPossibleGenerics(l.all[19])
        )
        21 -> Tuple21.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4]),
            lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]),
            lookupArbyWithPossibleGenerics(l.all[7]),
            lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]),
            lookupArbyWithPossibleGenerics(l.all[10]),
            lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]),
            lookupArbyWithPossibleGenerics(l.all[13]),
            lookupArbyWithPossibleGenerics(l.all[14]),
            lookupArbyWithPossibleGenerics(l.all[15]),
            lookupArbyWithPossibleGenerics(l.all[16]),
            lookupArbyWithPossibleGenerics(l.all[17]),
            lookupArbyWithPossibleGenerics(l.all[18]),
            lookupArbyWithPossibleGenerics(l.all[19]),
            lookupArbyWithPossibleGenerics(l.all[20])
        )
        22 -> Tuple22.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]),
            lookupArbyWithPossibleGenerics(l.all[1]),
            lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]),
            lookupArbyWithPossibleGenerics(l.all[4]),
            lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]),
            lookupArbyWithPossibleGenerics(l.all[7]),
            lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]),
            lookupArbyWithPossibleGenerics(l.all[10]),
            lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]),
            lookupArbyWithPossibleGenerics(l.all[13]),
            lookupArbyWithPossibleGenerics(l.all[14]),
            lookupArbyWithPossibleGenerics(l.all[15]),
            lookupArbyWithPossibleGenerics(l.all[16]),
            lookupArbyWithPossibleGenerics(l.all[17]),
            lookupArbyWithPossibleGenerics(l.all[18]),
            lookupArbyWithPossibleGenerics(l.all[19]),
            lookupArbyWithPossibleGenerics(l.all[20]),
            lookupArbyWithPossibleGenerics(l.all[21])
        )
        else -> throw java.lang.IllegalStateException("Unsupported tuple size > 21")
    } as Arbitrary<A>

fun lookupArbyByName(s: String): Arbitrary<*> = lookupArby<Any>(s)
fun className(n: Int, l: Nel<Type>): String = ((l.all[n] as WildcardType).upperBounds[0] as Class<*>).name