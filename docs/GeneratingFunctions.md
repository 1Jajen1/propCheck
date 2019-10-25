# Generating functions

## Table of contents
* [Intro](https://github.com/1Jajen1/propCheck/blob/master/docs/GeneratingFunctions.md#Intro)
* [Coarbitrary and Func](https://github.com/1Jajen1/propCheck/blob/master/docs/GeneratingFunctions.md#coarbitrarya-and-funca)
    + [Coarbitrary](https://github.com/1Jajen1/propCheck/blob/master/docs/GeneratingFunctions.md#coarbitrary)
    * [Func](https://github.com/1Jajen1/propCheck/blob/master/docs/GeneratingFunctions.md#func)

### Intro

When testing higher order functions (like map, fold etc) we sometimes need to generate arbitrary functions that are still deterministic.
This could be done by just generating constant functions, but that misses out on possible errors. Also we'd like to have functions that can be shown and shrunk to actually understand what is going on if it fails.

Generating functions using the Arbitrary instance for `Fun<A, B>` does exactly that.

```kotlin
propCheck {
    forAll { opt: Option<Int> ->
        forAll { (f): Fun<Int, Long> ->
            forAll { (g): Fun<Long, Int> ->
                opt.map(f).map(g) == opt.map(f andThen g)
            }
        }
    }
}
// prints =>
+++ OK, passed 100 tests.
``` 

```kotlin
propCheck {
    forAll { opt: Option<Int> ->
        forAll { (f): Fun<Int, Long> ->
            opt.map(f) == opt.fold({ none<Long>() }, { f(it + 1).some() })
        }
    }
}
// prints =>
*** Failed! (after 4 tests and 67 shrinks):
Falsifiable
Some(0)
[1 -> 1, _ -> 0]
```

### `Coarbitrary<A> and Func<A>`

The `Arbitrary` instance for `Func<A, B>` requires a `Func<A>` and `Coarbitrary<A>` instance (alongside an `Arbitrary<B>`). These are implemented for some basic types, just as `Arbitrary` is, but that is likely not enough.

#### Coarbitrary
Coarbitrary is a bit easier to implement:
```kotlin
interface Coarbitrary<A> {
    fun <B> Gen<B>.coarbitrary(a: A): Gen<B>
}
```

The intuition for `coarbitrary` is that `A` varies the output of `Gen<B>` such that different `A`'s lead to different `B`'s.

This is usually implemented by either mapping the value `A` to something that already has a `Coarbitrary` instance or by using `Gen<B>.variant(i: Long): Gen<B>` and mapping `A` to some long.

For example: `Option.coarbitrary`
```kotlin
interface OptionCoarbitrary<A> : Coarbitrary<Option<A>> {
    fun CA(): Coarbitrary<A>

    override fun <B> Gen<B>.coarbitrary(a: Option<A>): Gen<B> = a.fold({
        variant(0)
    }, { a ->
        CA().run { coarbitrary(a).variant(1) }
    })
}
```

#### Func

```kotlin
interface Func<A> {
    fun <B> function(f: (A) -> B): Fn<A, B>
}
```

This is also implemented for most basic types, and implementing it should always be done by using the combinators listed below and not by manually implementing it.

Combinators:
- `funMap(funcB: Func<B>, from: (A) -> B, to: (B) -> A, f: (A) -> C): Fn<A, C>` map `A` to `B` which already has a `Func` instance.
- `funPair(funcA: Func<A>, funcB: Func<B>, f: (Tuple2<A, B>) -> C): Fn<Tuple2<A, B>, C>` useful for implementing uncurried functions
- `funEither(funcL: Func<L>, funcR: Func<R>, f: (Either<L, R>) -> C): Fn<Either<L, R>, C>`
- `unitFunc(): Func<Unit>`
- `<A: Enum<A>>funEnum(f: (A) -> B): Fn<A, B>`
- `funList(l: Collection<A>, f: (A) -> B): Fn<A, B>`

An example for an instance is NonEmptyList:
```kotlin
interface NonEmptyListFunc<A> : Func<NonEmptyList<A>> {
    fun AF(): Func<A>
    override fun <B> function(f: (NonEmptyList<A>) -> B): Fn<NonEmptyList<A>, B> =
        funMap(Tuple2.func(AF(), ListK.func(AF())), {
            Tuple2(it.head, it.tail.k())
        }, { (h, t) ->
            NonEmptyList(h, t)
        }, f)
}
```