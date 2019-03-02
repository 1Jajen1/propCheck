package io.jannis.propTest

import arrow.core.*
import arrow.data.ListK
import arrow.data.MapK
import arrow.data.SetK
import io.jannis.propTest.asciistring.arbitrary.arbitrary
import io.jannis.propTest.blind.arbitrary.arbitrary
import io.jannis.propTest.fixed.arbitrary.arbitrary
import io.jannis.propTest.instances.arbitrary
import io.jannis.propTest.instances.listk.arbitrary.arbitrary
import io.jannis.propTest.instances.mapk.arbitrary.arbitrary
import io.jannis.propTest.instances.setk.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple10.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple11.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple12.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple13.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple14.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple15.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple16.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple17.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple18.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple19.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple2.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple20.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple21.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple22.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple3.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple4.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple5.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple6.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple7.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple8.arbitrary.arbitrary
import io.jannis.propTest.instances.tuple9.arbitrary.arbitrary
import io.jannis.propTest.shrink2.arbitrary.arbitrary
import io.jannis.propTest.smart.arbitrary.arbitrary
import io.jannis.propTest.unicodestring.arbitrary.arbitrary
import java.lang.reflect.ParameterizedType
import java.lang.reflect.Type
import java.lang.reflect.WildcardType

fun <A : Any> lookupArby(qualifiedName: String): Arbitrary<A> = when (qualifiedName) {
    "kotlin.Int", "java.lang.Integer" -> Int.arbitrary()
    "kotlin.Long", "java.lang.Long" -> Long.arbitrary()
    "kotlin.String", "java.lang.String" -> String.arbitrary()
    "kotlin.Char", "java.lang.Char" -> Char.arbitrary()
    "kotlin.Float", "java.lang.Float" -> Float.arbitrary()
    "kotlin.Double", "java.lang.Double" -> Double.arbitrary()
    "kotlin.Boolean", "java.lang.Boolean" -> Boolean.arbitrary()
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

fun <A : Any> lookupArbyWithGenerics(klass: Class<A>, l: Nel<Type>): Arbitrary<A> =
    klass.name.let { name ->
        when { // I am keeping the KClass qualified names for now because i might want to use them later ...
            name.startsWith("arrow.core.Tuple") -> lookupTuple(klass, Integer.parseInt("${name[16]}"), l)
            name == Pair::class.qualifiedName || name == Pair::class.java.name -> lookupPair(klass, l)
            name == Triple::class.qualifiedName || name == Triple::class.java.name -> lookupTriple(klass, l)
            name == List::class.qualifiedName || name == ListK::class.qualifiedName ||
                    name == List::class.java.name || name == ListK::class.java.name -> lookupList(klass, l)
            name == Set::class.qualifiedName || name == SetK::class.qualifiedName ||
                    name == Set::class.java.name || name == SetK::class.java.name -> lookupSet(klass, l)
            name == Map::class.qualifiedName || name == MapK::class.qualifiedName ||
                    name == Map::class.java.name || name == MapK::class.java.name -> lookupMap(klass, l)
            name == Blind::class.qualifiedName || name == Blind::class.java.name -> lookupBlind(klass, l)
            name == Fixed::class.qualifiedName || name == Fixed::class.java.name -> lookupFixed(klass, l)
            name == Smart::class.qualifiedName || name == Smart::class.java.name -> lookupSmart(klass, l)
            name == Shrink2::class.qualifiedName || name == Shrink2::class.java.name -> lookupShrink2(klass, l)
            else -> throw IllegalStateException("Unsupported class $name")
        }
    }

fun <A: Any> lookupShrink2(klass: Class<A>, l: Nel<Type>): Arbitrary<A> =
    if (l.size != 1) throw IllegalStateException("Could not find default arbitrary for ${klass.name}")
    else Shrink2.arbitrary(lookupArbyWithPossibleGenerics(l.head)) as Arbitrary<A>

fun <A: Any> lookupSmart(klass: Class<A>, l: Nel<Type>): Arbitrary<A> =
    if (l.size != 1) throw IllegalStateException("Could not find default arbitrary for ${klass.name}")
    else Smart.arbitrary(lookupArbyWithPossibleGenerics(l.head)) as Arbitrary<A>

fun <A: Any> lookupFixed(klass: Class<A>, l: Nel<Type>): Arbitrary<A> =
    if (l.size != 1) throw IllegalStateException("Could not find default arbitrary for ${klass.name}")
    else Fixed.arbitrary(lookupArbyWithPossibleGenerics(l.head)) as Arbitrary<A>

fun <A: Any> lookupBlind(klass: Class<A>, l: Nel<Type>): Arbitrary<A> =
    if (l.size != 1) throw IllegalStateException("Could not find default arbitrary for ${klass.name}")
    else Blind.arbitrary(lookupArbyWithPossibleGenerics(l.head)) as Arbitrary<A>

fun <A: Any> lookupMap(klass: Class<A>, l: Nel<Type>): Arbitrary<A> =
    if (l.size != 2) throw IllegalStateException("Could not find default arbitrary for ${klass.name}")
    else MapK.arbitrary(
        lookupArbyWithPossibleGenerics(l.head),
        lookupArbyWithPossibleGenerics(l.tail[0])
    ) as Arbitrary<A>

fun <A: Any> lookupSet(klass: Class<A>, l: Nel<Type>): Arbitrary<A> =
    if (l.size != 1) throw IllegalStateException("Could not find default arbitrary for ${klass.name}")
    else SetK.arbitrary(lookupArbyWithPossibleGenerics(l.all[0])) as Arbitrary<A>

fun <A: Any> lookupList(klass: Class<A>, l: Nel<Type>): Arbitrary<A> =
    if (l.size != 1) throw IllegalStateException("Could not find default arbitrary for ${klass.name}")
    else ListK.arbitrary(lookupArbyWithPossibleGenerics(l.all[0])) as Arbitrary<A>

fun <A: Any> lookupPair(klass: Class<A>, l: Nel<Type>): Arbitrary<A> =
    if (l.size != 2) throw IllegalStateException("Could not find default arbitrary for ${klass.name}")
    else object : Arbitrary<Pair<Any, Any>> {
        val tupArb = lookupTuple(klass, 2, l)
        override fun arbitrary(): Gen<Pair<Any, Any>> = tupArb.arbitrary().map { (it as Tuple2<Any, Any>).let { it.a to it.b } }
        override fun shrink(fail: Pair<Any, Any>): Sequence<Pair<Any, Any>> = tupArb.shrink((fail.first toT fail.second) as A)
            .map { (it as Tuple2<Any, Any>).let { it.a to it.b } }
    } as Arbitrary<A>

fun <A: Any> lookupTriple(klass: Class<A>, l: Nel<Type>): Arbitrary<A> =
    if (l.size != 3) throw IllegalStateException("Could not find default arbitrary for ${klass.name}")
    else object : Arbitrary<Triple<Any, Any, Any>> {
        val tupArb = lookupTuple(klass, 3, l)
        override fun arbitrary(): Gen<Triple<Any, Any, Any>> = tupArb.arbitrary().map { (it as Tuple3<Any, Any, Any>).let { Triple(it.a, it.b, it.c) } }
        override fun shrink(fail: Triple<Any, Any, Any>): Sequence<Triple<Any, Any, Any>> = tupArb.shrink((Tuple3(fail.first, fail.second, fail.third)) as A)
            .map { (it as Tuple3<Any, Any, Any>).let { Triple(it.a, it.b, it.c) } }
    } as Arbitrary<A>

fun <A : Any> lookupTuple(klass: Class<A>, n: Int, l: Nel<Type>): Arbitrary<A> =
    if (l.size != n) throw IllegalStateException("Could not find default arbitrary for ${klass.name}")
    else when (n) {
        2 -> Tuple2.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1])
        )
        3 -> Tuple3.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2])
        )
        4 -> Tuple4.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3])
        )
        5 -> Tuple5.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4])
        )
        6 -> Tuple6.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4]), lookupArbyWithPossibleGenerics(l.all[5])
        )
        7 -> Tuple7.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4]), lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[5])
        )
        8 -> Tuple8.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4]), lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[5]), lookupArbyWithPossibleGenerics(l.all[6])
        )
        9 -> Tuple9.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4]), lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]), lookupArbyWithPossibleGenerics(l.all[7]), lookupArbyWithPossibleGenerics(l.all[8])
        )
        10 -> Tuple10.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4]), lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]), lookupArbyWithPossibleGenerics(l.all[7]), lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9])
        )
        11 -> Tuple11.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4]), lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]), lookupArbyWithPossibleGenerics(l.all[7]), lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]), lookupArbyWithPossibleGenerics(l.all[10])
        )
        12 -> Tuple12.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4]), lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]), lookupArbyWithPossibleGenerics(l.all[7]), lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]), lookupArbyWithPossibleGenerics(l.all[10]), lookupArbyWithPossibleGenerics(l.all[11])
        )
        13 -> Tuple13.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4]), lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]), lookupArbyWithPossibleGenerics(l.all[7]), lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]), lookupArbyWithPossibleGenerics(l.all[10]), lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12])
        )
        14 -> Tuple14.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4]), lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]), lookupArbyWithPossibleGenerics(l.all[7]), lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]), lookupArbyWithPossibleGenerics(l.all[10]), lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]), lookupArbyWithPossibleGenerics(l.all[13])
        )
        15 -> Tuple15.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4]), lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]), lookupArbyWithPossibleGenerics(l.all[7]), lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]), lookupArbyWithPossibleGenerics(l.all[10]), lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]), lookupArbyWithPossibleGenerics(l.all[13]), lookupArbyWithPossibleGenerics(l.all[14])
        )
        16 -> Tuple16.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4]), lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]), lookupArbyWithPossibleGenerics(l.all[7]), lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]), lookupArbyWithPossibleGenerics(l.all[10]), lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]), lookupArbyWithPossibleGenerics(l.all[13]), lookupArbyWithPossibleGenerics(l.all[14]),
            lookupArbyWithPossibleGenerics(l.all[15])
        )
        17 -> Tuple17.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4]), lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]), lookupArbyWithPossibleGenerics(l.all[7]), lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]), lookupArbyWithPossibleGenerics(l.all[10]), lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]), lookupArbyWithPossibleGenerics(l.all[13]), lookupArbyWithPossibleGenerics(l.all[14]),
            lookupArbyWithPossibleGenerics(l.all[15]), lookupArbyWithPossibleGenerics(l.all[16])
        )
        18 -> Tuple18.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4]), lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]), lookupArbyWithPossibleGenerics(l.all[7]), lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]), lookupArbyWithPossibleGenerics(l.all[10]), lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]), lookupArbyWithPossibleGenerics(l.all[13]), lookupArbyWithPossibleGenerics(l.all[14]),
            lookupArbyWithPossibleGenerics(l.all[15]), lookupArbyWithPossibleGenerics(l.all[16]), lookupArbyWithPossibleGenerics(l.all[17])
        )
        19 -> Tuple19.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4]), lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]), lookupArbyWithPossibleGenerics(l.all[7]), lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]), lookupArbyWithPossibleGenerics(l.all[10]), lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]), lookupArbyWithPossibleGenerics(l.all[13]), lookupArbyWithPossibleGenerics(l.all[14]),
            lookupArbyWithPossibleGenerics(l.all[15]), lookupArbyWithPossibleGenerics(l.all[16]), lookupArbyWithPossibleGenerics(l.all[17]),
            lookupArbyWithPossibleGenerics(l.all[18])
        )
        20 -> Tuple20.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4]), lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]), lookupArbyWithPossibleGenerics(l.all[7]), lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]), lookupArbyWithPossibleGenerics(l.all[10]), lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]), lookupArbyWithPossibleGenerics(l.all[13]), lookupArbyWithPossibleGenerics(l.all[14]),
            lookupArbyWithPossibleGenerics(l.all[15]), lookupArbyWithPossibleGenerics(l.all[16]), lookupArbyWithPossibleGenerics(l.all[17]),
            lookupArbyWithPossibleGenerics(l.all[18]), lookupArbyWithPossibleGenerics(l.all[19])
        )
        21 -> Tuple21.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4]), lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]), lookupArbyWithPossibleGenerics(l.all[7]), lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]), lookupArbyWithPossibleGenerics(l.all[10]), lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]), lookupArbyWithPossibleGenerics(l.all[13]), lookupArbyWithPossibleGenerics(l.all[14]),
            lookupArbyWithPossibleGenerics(l.all[15]), lookupArbyWithPossibleGenerics(l.all[16]), lookupArbyWithPossibleGenerics(l.all[17]),
            lookupArbyWithPossibleGenerics(l.all[18]), lookupArbyWithPossibleGenerics(l.all[19]), lookupArbyWithPossibleGenerics(l.all[20])
        )
        22 -> Tuple22.arbitrary(
            lookupArbyWithPossibleGenerics(l.all[0]), lookupArbyWithPossibleGenerics(l.all[1]), lookupArbyWithPossibleGenerics(l.all[2]),
            lookupArbyWithPossibleGenerics(l.all[3]), lookupArbyWithPossibleGenerics(l.all[4]), lookupArbyWithPossibleGenerics(l.all[5]),
            lookupArbyWithPossibleGenerics(l.all[6]), lookupArbyWithPossibleGenerics(l.all[7]), lookupArbyWithPossibleGenerics(l.all[8]),
            lookupArbyWithPossibleGenerics(l.all[9]), lookupArbyWithPossibleGenerics(l.all[10]), lookupArbyWithPossibleGenerics(l.all[11]),
            lookupArbyWithPossibleGenerics(l.all[12]), lookupArbyWithPossibleGenerics(l.all[13]), lookupArbyWithPossibleGenerics(l.all[14]),
            lookupArbyWithPossibleGenerics(l.all[15]), lookupArbyWithPossibleGenerics(l.all[16]), lookupArbyWithPossibleGenerics(l.all[17]),
            lookupArbyWithPossibleGenerics(l.all[18]), lookupArbyWithPossibleGenerics(l.all[19]), lookupArbyWithPossibleGenerics(l.all[20]),
            lookupArbyWithPossibleGenerics(l.all[21])
        )
        else -> throw java.lang.IllegalStateException("Unsupported tuple size > 21")
    } as Arbitrary<A>

fun lookupArbyByName(s: String): Arbitrary<*> = lookupArby<Any>(s)
fun className(n: Int, l: Nel<Type>): String = ((l.all[n] as WildcardType).upperBounds[0] as Class<*>).name