package propCheck.instances

import arrow.core.*
import arrow.extension
import arrow.typeclasses.Show
import propCheck.arbitrary.*
import propCheck.arbitrary.tuple2.func.func
import propCheck.instances.listk.func.func
import propCheck.instances.tuple2.arbitrary.arbitrary

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

@extension
interface MapKFunc<K, V> : Func<MapK<K, V>> {
    fun KF(): Func<K>
    fun VF(): Func<V>

    override fun <B> function(f: (MapK<K, V>) -> B): Fn<MapK<K, V>, B> =
        funMap(ListK.func(Tuple2.func(KF(), VF())), {
            it.toList().map { it.toTuple2() }.k()
        }, {
            it.toMap().k()
        }, f)
}
