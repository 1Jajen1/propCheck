package io.jannis.propTest

import arrow.core.Tuple2
import arrow.core.toT
import arrow.data.Nel
import arrow.test.laws.MonadLaws
import arrow.typeclasses.Eq
import io.jannis.propTest.assertions.forAllP
import io.jannis.propTest.assertions.idempotentIOProperty
import io.jannis.propTest.gen.monad.monad

class GenSpec : LawSpec() {
    init {
        testLaws(
            MonadLaws.laws(
                Gen.monad(),
                Eq { a, b ->
                    a.fix().unGen(0L toT 10) == b.fix().unGen(0L toT 10)
                }
            )
        )

        "Gen.resize should work" {
            propCheck {
                forAllP<NonNegative<Int>>().invoke { (i) ->
                    idempotentIOProperty().invoke(
                        Gen.getSize().resize(i).generate().map { it == i }
                    )
                }
            }
        }

        "Gen.scale should work" {
            // TODO redo when generating functions is done
            propCheck {
                forAllP<Tuple2<NonNegative<Int>, Positive<Int>>>().invoke { (iP, jP) ->
                    val (i) = iP; val (j) = jP
                    idempotentIOProperty().invoke(
                        Gen.getSize().scale { it + j }.resize(i).generate().map { it == i + j }
                    )
                }
            }
        }

        /* TODO Fix stack safety of Gen so that these functions work under any circumstance
        "Gen.suchThat" {
            // TODO redo when generating functions is done
            propCheck {
                forAllP<Int>().invoke { i ->
                    idempotentIOProperty().invoke(
                        Gen.getSize().suchThat { it > i }.generate().map { it > i }
                    )
                }
            }
        }

        "Gen.suchThatOption" {
            // TODO redo when generating functions is done
            propCheck {
                forAllP<Int>().invoke { i ->
                    idempotentIOProperty().invoke(
                        Gen.getSize().suchThatOption { it > i }.generate().map {
                            it.fold({ true }, { it > i })
                        }
                    )
                }
            }
        }

        "Gen.suchThatMap" {
            // TODO redo when generating functions is done
            propCheck {
                forAllP<Tuple2<Positive<Int>, Option<Positive<Int>>>>().invoke { (iP, jOptP) ->
                    val (i) = iP;
                    idempotentIOProperty().invoke(
                        Gen.getSize().suchThatMap { s -> jOptP.map { it.a + s } }.resize(i).generate().map { s ->
                            jOptP.fold({ false }, { (it) -> it + i == s })
                        }
                    )
                }
            }
        }
        */

        "Gen.listOf should generate lists of size 0 to i" {
            propCheck {
                forAllP<NonNegative<Int>>().invoke { (i) ->
                    idempotentIOProperty().invoke(
                        arbitrarySizedByte().listOf().resize(i).generate().map { it.size <= i }
                    )
                }
            }
        }

        "Gen.nelOf should generate Nonemptylists of size 1 to i" {
            propCheck {
                forAllP<Positive<Int>>().invoke { (i) ->
                    idempotentIOProperty().invoke(
                        arbitrarySizedByte().nelOf().resize(i).generate().map { it.size <= i }
                    )
                }
            }
        }

        "Gen.vectorOf should generate lists of size n" {
            propCheck {
                forAllP<NonNegative<Int>>().invoke { (i) ->
                    idempotentIOProperty().invoke(
                        arbitrarySizedByte().vectorOf(i).generate().map { it.size == i }
                    )
                }
            }
        }

        "Gen.sized should work" {
            propCheck {
                forAllP<NonNegative<Int>>().invoke { (i) ->
                    idempotentIOProperty().invoke(
                        Gen.sized { Gen.getSize().resize(it) }.resize(i).generate().map { it == i }
                    )
                }
            }
        }

        "Gen.getSize should work" {
            propCheck {
                forAllP<NonNegative<Int>>().invoke { (i) ->
                    idempotentIOProperty().invoke(
                        Gen.getSize().resize(i).generate().map { it == i }
                    )
                }
            }
        }

        "Gen.choose should work" {
            propCheck {
                forAllP<Tuple2<Int, Int>>().invoke { (a, b) ->
                    val l = Math.min(a, b);
                    val u = Math.max(a, b)
                    idempotentIOProperty().invoke(
                        Gen.choose(l toT u, Int.random()).generate().map { it in l..u }
                    )
                }
            }
        }

        "Gen.elements should return elemets of a given list" {
            propCheck {
                forAllP<Nel<Int>>().invoke { l ->
                    idempotentIOProperty().invoke(
                        Gen.elements(*l.all.toTypedArray()).generate().map { l.contains(it) }
                    )
                }
            }
        }

        "Gen.sublistOf should return only sublists" {
            propCheck {
                forAllP<List<Int>>().invoke { l ->
                    idempotentIOProperty().invoke(
                        Gen.sublistOf(l).generate().map { it.map { l.contains(it) }.fold(true) { acc, v -> acc && v } }
                    )
                }
            }
        }

        "Gen.shuffle should create permutations of a list" {
            propCheck {
                forAllP<List<Int>>().invoke {  l ->
                    idempotentIOProperty().invoke(
                        Gen.shuffle(l).generate().map { it.containsAll(l) }
                    )
                }
            }
        }
    }
}