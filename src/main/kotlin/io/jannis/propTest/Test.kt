package io.jannis.propTest

import io.jannis.propTest.assertions.forAllShrink

fun main() {
    propCheck {
        forAllShrink<Triple<ByteArray, ByteArray, ByteArray>>().invoke {
            it.first.sum() <= it.second.sum() && it.second.sum() <= it.third.sum()
        }
    }
}