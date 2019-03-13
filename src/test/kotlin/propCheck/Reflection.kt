package propCheck

import arrow.core.*
import arrow.data.*
import io.kotlintest.specs.WordSpec
import propCheck.arbitrary.*
import propCheck.arbitrary.asciistring.arbitrary.arbitrary
import propCheck.arbitrary.blind.arbitrary.arbitrary
import propCheck.arbitrary.fixed.arbitrary.arbitrary
import propCheck.instances.*
import propCheck.instances.either.arbitrary.arbitrary
import propCheck.instances.id.arbitrary.arbitrary
import propCheck.instances.ior.arbitrary.arbitrary
import propCheck.instances.listk.arbitrary.arbitrary
import propCheck.instances.mapk.arbitrary.arbitrary
import propCheck.instances.nonemptylist.arbitrary.arbitrary
import propCheck.instances.option.arbitrary.arbitrary
import propCheck.instances.setk.arbitrary.arbitrary
import propCheck.instances.tuple2.arbitrary.arbitrary
import propCheck.instances.tuple4.arbitrary.arbitrary
import propCheck.instances.validated.arbitrary.arbitrary
import propCheck.arbitrary.negative.arbitrary.arbitrary
import propCheck.arbitrary.nonnegative.arbitrary.arbitrary
import propCheck.arbitrary.nonpositive.arbitrary.arbitrary
import propCheck.arbitrary.positive.arbitrary.arbitrary
import propCheck.arbitrary.shrink2.arbitrary.arbitrary
import propCheck.arbitrary.smart.arbitrary.arbitrary
import propCheck.arbitrary.unicodestring.arbitrary.arbitrary

class ReflectionSpec : WordSpec({
    "reflection should be able to lookup " should {
        "int arbitrary instances" {
            defArbitrary<Int>() == Int.arbitrary()
        }
        "long arbitrary instances" {
            defArbitrary<Long>() == Long.arbitrary()
        }
        "byte arbitrary instances" {
            defArbitrary<Byte>() == Byte.arbitrary()
        }
        "float arbitrary instances" {
            defArbitrary<Float>() == Float.arbitrary()
        }
        "double arbitrary instances" {
            defArbitrary<Double>() == Double.arbitrary()
        }
        "boolean arbitrary instances" {
            defArbitrary<Boolean>() == Boolean.arbitrary()
        }
        "char arbitrary instances" {
            defArbitrary<Char>() == Char.arbitrary()
        }
        "string arbitrary instances" {
            defArbitrary<String>() == String.arbitrary()
        }
        "IntArray arbitrary instances" {
            defArbitrary<IntArray>() == intArrayArb
        }
        "LongArray arbitrary instances" {
            defArbitrary<LongArray>() == longArrayArb
        }
        "ByteArray arbitrary instances" {
            defArbitrary<ByteArray>() == byteArrayArb
        }
        "FloatArray arbitrary instances" {
            defArbitrary<FloatArray>() == floatArrayArb
        }
        "DoubleArray arbitrary instances" {
            defArbitrary<DoubleArray>() == doubleArrayArb
        }
        "BooleanArray arbitrary instances" {
            defArbitrary<BooleanArray>() == booleanArrayArb
        }
        "ASCIIString arbitrary instances" {
            defArbitrary<ASCIIString>() == ASCIIString.arbitrary()
        }
        "UnicodeString arbitrary instances" {
            defArbitrary<UnicodeString>() == UnicodeString.arbitrary()
        }
        "TupleN arbitrary instances" {
            defArbitrary<Tuple2<Int, Int>>() == Tuple2.arbitrary(Int.arbitrary(), Int.arbitrary())
            defArbitrary<Tuple4<Int, String, String, Int>>() == Tuple4.arbitrary(Int.arbitrary(), String.arbitrary(), String.arbitrary(), Int.arbitrary())
        }
        "Pair arbitrary instaces" {
            // TODO
            defArbitrary<Pair<Long, Byte>>()
        }
        "Triple arbitrary instaces" {
            // TODO
            defArbitrary<Triple<Float, Long, Byte>>()
        }
        "List arbitrary insances" {
            defArbitrary<List<Float>>() == ListK.arbitrary(Float.arbitrary())
            defArbitrary<ListK<Double>>() == ListK.arbitrary(Double.arbitrary())
        }
        "Set arbitrary instances" {
            defArbitrary<Set<Float>>() == SetK.arbitrary(Float.arbitrary())
            defArbitrary<SetK<Double>>() == SetK.arbitrary(Double.arbitrary())
        }
        "Map arbitrary instances" {
            defArbitrary<Map<Byte, Float>>() == MapK.arbitrary(Byte.arbitrary(), Float.arbitrary())
            defArbitrary<MapK<Int, Double>>() == MapK.arbitrary(Int.arbitrary(), Double.arbitrary())
        }
        "Blind arbitrary instances" {
            defArbitrary<Blind<IntArray>>() == Blind.arbitrary(intArrayArb)
        }
        "Fixed arbitrary instances" {
            defArbitrary<Fixed<Boolean>>() == Fixed.arbitrary(Boolean.arbitrary())
        }
        "Smart arbitrary instances" {
            defArbitrary<Smart<Long>>() == Smart.arbitrary(Long.arbitrary())
        }
        "Shrink2 arbitrary instances" {
            defArbitrary<Tuple2<Int, Int>>() == Shrink2.arbitrary<Tuple2<Int, Int>>(defArbitrary())
        }
        "Either arbitrary instances" {
            defArbitrary<Either<String, Int>>() == Either.arbitrary(String.arbitrary(), Int.arbitrary())
        }
        "Option arbitrary instances" {
            defArbitrary<Option<Boolean>>() == Option.arbitrary(Boolean.arbitrary())
        }
        "Id arbitrary instances" {
            defArbitrary<Id<Byte>>() == Id.arbitrary(Byte.arbitrary())
        }
        "Ior arbitrary instances" {
            defArbitrary<Ior<String, Long>>() == Ior.arbitrary(String.arbitrary(), Long.arbitrary())
        }
        "Nel arbitrary instances" {
            defArbitrary<Nel<Int>>() == Nel.arbitrary(Int.arbitrary())
        }
        "Validated arbitrary instances" {
            defArbitrary<Validated<Long, String>>() == Validated.arbitrary(Long.arbitrary(), String.arbitrary())
        }
        "Positive arbitrary instances" {
            defArbitrary<Positive<Byte>>() == Positive.arbitrary(Byte.arbitrary())
        }
        "NonNegative arbitrary instances" {
            defArbitrary<NonNegative<Long>>() == NonNegative.arbitrary(Long.arbitrary())
        }
        "Negative arbitrary instances" {
            defArbitrary<Negative<Float>>() == Negative.arbitrary(Float.arbitrary())
        }
        "NonPostiive arbitrary instances" {
            defArbitrary<NonPositive<Double>>() == NonPositive.arbitrary(Double.arbitrary())
        }
        "nested arbitrary instances compose from the above" {
            defArbitrary<Tuple4<List<Positive<Int>>, Set<String>, MapK<Tuple2<Int, Long>, UnicodeString>, FloatArray>>() ==
                    Tuple4.arbitrary(
                        ListK.arbitrary(Positive.arbitrary(Int.arbitrary())),
                        SetK.arbitrary(String.arbitrary()),
                        MapK.arbitrary(
                            Tuple2.arbitrary(Int.arbitrary(), Long.arbitrary()),
                            UnicodeString.arbitrary()
                        ),
                        floatArrayArb
                    )
        }
    }
})