package propCheck.instances

import arrow.core.Tuple2
import arrow.core.toT
import arrow.data.MapK
import arrow.data.mapOf
import arrow.extension
import arrow.typeclasses.Show
import propCheck.arbitrary.Arbitrary
import propCheck.arbitrary.Gen
import propCheck.instances.tuple2.arbitrary.arbitrary
import propCheck.arbitrary.shrinkList

@extension
interface MapKArbitrary<K, V> : Arbitrary<MapK<K, V>> {
    fun AK(): Arbitrary<K>
    fun AV(): Arbitrary<V>

    override fun arbitrary(): Gen<MapK<K, V>> = Tuple2.arbitrary(AK(), AV()).arbitrary().listOf()
        .map { l -> mapOf(*l.toTypedArray()) }

    override fun shrink(fail: MapK<K, V>): Sequence<MapK<K, V>> = shrinkList(fail.toList().map { (a, b) -> a toT b }) {
        Tuple2.arbitrary(AK(), AV()).shrink(it)
    }.map { mapOf(*it.toTypedArray()) }
}

interface MapKShow<K, V> : Show<MapK<K, V>> {
    fun SK(): Show<K>
    fun SV(): Show<V>
    override fun MapK<K, V>.show(): String =
            "Map(" + entries.joinToString { (k, v) -> SK().run { k.show() } + " -> " + SV().run { v.show() } } + ")"
}