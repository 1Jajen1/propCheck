package io.jannis.propTest.instances

import io.jannis.propTest.Arbitrary
import io.jannis.propTest.Gen
import io.jannis.propTest.shrinkList
import arrow.core.Tuple2
import arrow.core.toT
import arrow.data.MapK
import arrow.data.mapOf
import arrow.extension

/*
@extension
interface MapKArbitrary<K, V> : Arbitrary<MapK<K, V>> {
    fun AK(): Arbitrary<K>
    fun AV(): Arbitrary<V>

    override fun arbitrary(): Gen<MapK<K, V>> = Tuple2.arbitrary(AK(), AV()).arbitrary().listOf()
        .map { l -> mapOf(*l.toTypedArray()) }

    override fun shrink(fail: MapK<K, V>): Sequence<MapK<K, V>> = shrinkList<Tuple2<K, V>> {
        Tuple2.arbitrary(AK(), AV()).shrink(it)
    }.invoke(fail.toList().map { (a, b) -> a toT b }).map { mapOf(*it.toTypedArray()) }
}*/