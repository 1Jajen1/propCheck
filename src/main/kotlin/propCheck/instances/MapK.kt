package propCheck.instances

import arrow.core.Tuple2
import arrow.core.toT
import arrow.data.MapK
import arrow.data.mapOf
import arrow.extension
import propCheck.Arbitrary
import propCheck.Gen
import propCheck.instances.tuple2.arbitrary.arbitrary
import propCheck.shrinkList

@extension
interface MapKArbitrary<K, V> : Arbitrary<MapK<K, V>> {
    fun AK(): Arbitrary<K>
    fun AV(): Arbitrary<V>

    override fun arbitrary(): Gen<MapK<K, V>> = Tuple2.arbitrary(AK(), AV()).arbitrary().listOf()
        .map { l -> mapOf(*l.toTypedArray()) }

    override fun shrink(fail: MapK<K, V>): Sequence<MapK<K, V>> = shrinkList<Tuple2<K, V>> {
        Tuple2.arbitrary(AK(), AV()).shrink(it)
    }.invoke(fail.toList().map { (a, b) -> a toT b }).map { mapOf(*it.toTypedArray()) }
}