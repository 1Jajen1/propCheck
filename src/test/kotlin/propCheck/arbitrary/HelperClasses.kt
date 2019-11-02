package propCheck.arbitrary

import arrow.core.ListK
import arrow.core.Tuple2
import arrow.core.extensions.eq
import arrow.core.extensions.listk.eq.eq
import arrow.core.extensions.order
import arrow.core.k
import arrow.core.toT
import arrow.test.laws.FunctorLaws
import arrow.typeclasses.Eq
import propCheck.*
import propCheck.arbitrary.blind.eq.eq
import propCheck.arbitrary.blind.functor.functor
import propCheck.arbitrary.blind.show.show
import propCheck.arbitrary.negative.arbitrary.arbitrary
import propCheck.arbitrary.negative.eq.eq
import propCheck.arbitrary.negative.functor.functor
import propCheck.arbitrary.nonnegative.arbitrary.arbitrary
import propCheck.arbitrary.nonnegative.eq.eq
import propCheck.arbitrary.nonnegative.functor.functor
import propCheck.arbitrary.nonpositive.arbitrary.arbitrary
import propCheck.arbitrary.nonpositive.eq.eq
import propCheck.arbitrary.nonpositive.functor.functor
import propCheck.arbitrary.orderedlist.arbitrary.arbitrary
import propCheck.arbitrary.orderedlist.eq.eq
import propCheck.arbitrary.orderedlist.functor.functor
import propCheck.arbitrary.positive.arbitrary.arbitrary
import propCheck.arbitrary.positive.eq.eq
import propCheck.arbitrary.positive.functor.functor
import propCheck.arbitrary.shrink2.arbitrary.arbitrary
import propCheck.arbitrary.shrink2.eq.eq
import propCheck.arbitrary.shrink2.functor.functor
import propCheck.arbitrary.shrinking.arbitrary.arbitrary
import propCheck.arbitrary.shrinking.functor.functor
import propCheck.arbitrary.smart.functor.functor
import propCheck.instances.arbitrary
import propCheck.property.cover
import propCheck.property.forAll
import propCheck.property.idempotentIOProperty
import propCheck.property.testable

class BlindSpec : LawSpec() {
    init {
        testLaws(
            FunctorLaws.laws(Blind.functor(), { Blind(it) }, Eq { a, b ->
                Blind.eq(Int.eq()).run { a.fix().eqv(b.fix()) }
            })
        )

        "Blind should display as (*) with show" {
            propCheck {
                forAll { i: Blind<Int> ->
                    Blind.show<Int>().run { i.show() } == "(*)"
                }
            }
        }
    }
}

class FixedSpec : LawSpec() {
    init {
        testLaws(
            FunctorLaws.laws(Blind.functor(), { Blind(it) }, Eq { a, b ->
                Blind.eq(Int.eq()).run { a.fix().eqv(b.fix()) }
            })
        )

        "Fixed should never be shrunk" {
            propCheck {
                forAll { _: Fixed<Int> ->
                    idempotentIOProperty(
                        propCheckIO { Boolean.testable().run { false.property() } }
                            .map {
                                (it is Result.Failure && it.numShrinks + it.numShrinkTries == 0)
                            }
                    )
                }
            }
        }
    }
}

class OrderedListSpec : LawSpec() {
    init {
        testLaws(
            FunctorLaws.laws(OrderedList.functor(), {
                OrderedList(listOf(it))
            }, Eq { a, b ->
                OrderedList.eq(Int.eq()).run { a.fix().eqv(b.fix()) }
            })
        )
        "OrderedList arbitrary should only return ordered lists" {
            propCheck {
                forAll(OrderedList.arbitrary(Int.order(), Int.arbitrary())) { (l) ->
                    propCheck.property.checkCoverage(
                        cover(85.0, l.size > 1, "non-trivial",
                            ListK.eq(Int.eq()).run {
                                l.sorted().k().eqv(l.k())
                            }
                        )
                    )
                }
            }
        }
        "OrderedList arbitrary should only shrink to ordered lists" {
            propCheck {
                forAll(OrderedList.arbitrary(Int.order(), Int.arbitrary())) { oL ->
                    OrderedList.arbitrary(Int.order(), Int.arbitrary()).shrink(oL)
                        .fold(true) { acc, (l) ->
                            acc && ListK.eq(Int.eq()).run {
                                l.sorted().k().eqv(l.k())
                            }
                        }
                }
            }
        }
    }
}

class SmartSpec : LawSpec() {
    init {
        testLaws(
            FunctorLaws.laws(Smart.functor(), { Smart(it, it) }, Eq { a, b ->
                val aF = a.fix(); val bF = b.fix()
                aF.a == bF.a && aF.i == bF.i
            })
        )
    }
}

class Shrink2Spec : LawSpec() {
    init {
        testLaws(
            FunctorLaws.laws(Shrink2.functor(), { Shrink2(it) }, Eq { a, b ->
                Shrink2.eq(Int.eq()).run { a.fix().eqv(b.fix()) }
            })
        )

        "shrink2 should perform 2 shrinking steps instead of just one" {
            val shrinkS = object : ShrinkState<Int, Int> {
                override fun shrinkInit(a: Int): Int = 0
                override fun shrinkState(a: Int, state: Int): Sequence<Tuple2<Int, Int>> =
                    (state + 1).let { sequenceOf(it toT it) }
            }
            propCheck {
                forAll(Shrink2.arbitrary(Shrinking.arbitrary(Int.arbitrary(), shrinkS))) { i ->
                    Shrink2.arbitrary(Shrinking.arbitrary(Int.arbitrary(), shrinkS)).shrink(i)
                        .fold(false) { acc, (v) -> acc || v.a == 2 }
                }
            }
        }
    }
}

class ShrinkingSpec : LawSpec() {
    init {
        testLaws(
            FunctorLaws.laws(Shrinking.functor(), {
                Shrinking(
                    0,
                    it
                )
            }, Eq { a, b ->
                val aF = a.fix(); val bF = b.fix()
                aF.a == bF.a && aF.s == bF.s
            })
        )

        "shrinking should shrink with keeping state" {
            val shrinkS = object : ShrinkState<Int, Int> {
                override fun shrinkInit(a: Int): Int = 0
                override fun shrinkState(a: Int, state: Int): Sequence<Tuple2<Int, Int>> =
                    (state + 1).let { sequenceOf(it toT it) }
            }
            propCheck {
                forAll(Shrinking.arbitrary(Int.arbitrary(), shrinkS)) { s ->
                    Shrinking.arbitrary(Int.arbitrary(), shrinkS).shrink(s).fold(true) { acc, (s, a) ->
                        acc && s == 1 && s == a
                    }
                }
            }
        }
    }
}

class PositiveSpec : LawSpec() {
    init {
        testLaws(
            FunctorLaws.laws(Positive.functor(), { Positive(it) }, Eq { a, b ->
                Positive.eq(Int.eq()).run { a.fix().eqv(b.fix()) }
            })
        )

        "positive arbitrary should only generate positive numbers" {
            propCheck {
                forAll { (i): Positive<Int> ->
                    i > 0
                }
            }
        }
        "positive arbitrary should only shrink to positive numbers" {
            propCheck {
                forAll { pI: Positive<Int> ->
                    Positive.arbitrary(Int.arbitrary()).shrink(pI).fold(true) { acc, (i) ->
                        acc && i > 0
                    }
                }
            }
        }
    }
}

class NonNegativeSpec : LawSpec() {
    init {
        testLaws(
            FunctorLaws.laws(NonNegative.functor(), { NonNegative(it) }, Eq { a, b ->
                NonNegative.eq(Int.eq()).run { a.fix().eqv(b.fix()) }
            })
        )

        "NonNegative arbitrary should only generate nonNegative numbers" {
            propCheck {
                forAll { (i): NonNegative<Int> ->
                    i >= 0
                }
            }
        }
        "NonNegative arbitrary should only shrink to nonNegative numbers" {
            propCheck {
                forAll { pI: NonNegative<Int> ->
                    NonNegative.arbitrary(Int.arbitrary()).shrink(pI).fold(true) { acc, (i) ->
                        acc && i >= 0
                    }
                }
            }
        }
    }
}

class NegativeSpec : LawSpec() {
    init {
        testLaws(
            FunctorLaws.laws(Negative.functor(), { Negative(it) }, Eq { a, b ->
                Negative.eq(Int.eq()).run { a.fix().eqv(b.fix()) }
            })
        )

        "Negative arbitrary should only generate Negative numbers" {
            propCheck {
                forAll { (i): Negative<Int> ->
                    i < 0
                }
            }
        }
        "Negative arbitrary should only shrink to Negative numbers" {
            propCheck {
                forAll { pI: Negative<Int> ->
                    Negative.arbitrary(Int.arbitrary()).shrink(pI).fold(true) { acc, (i) ->
                        acc && i < 0
                    }
                }
            }
        }
    }
}

class NonPositiveSpec : LawSpec() {
    init {
        testLaws(
            FunctorLaws.laws(NonPositive.functor(), { NonPositive(it) }, Eq { a, b ->
                NonPositive.eq(Int.eq()).run { a.fix().eqv(b.fix()) }
            })
        )

        "NonPositive arbitrary should only generate NonPositive numbers" {
            propCheck {
                forAll { (i): NonPositive<Int> ->
                    i <= 0
                }
            }
        }
        "NonPositive arbitrary should only shrink to NonPositive numbers" {
            propCheck {
                forAll { pI: NonPositive<Int> ->
                    NonPositive.arbitrary(Int.arbitrary()).shrink(pI).fold(true) { acc, (i) ->
                        acc && i <= 0
                    }
                }
            }
        }
    }
}