package io.jannis.propTest

import arrow.core.Tuple2
import arrow.data.ListK
import arrow.data.SetK
import io.jannis.propTest.assertions.forAllShrink

fun main() {
    propCheck {
        forAllShrink<IntArray>().invoke {
            it.sum() < 10
        }
    }
    propCheck {
        forAllShrink<List<Tuple2<ListK<SetK<Map<Int, Int>>>, Int>>>().invoke {
            it.sumBy { it.a.map { it.map { it.entries.map { it.value + it.key }.sum() }.sum() }.sum() + it.b } < 50
        }
    }
}