# Property testing in kotlin

## WIP Documentation and code itself is wip and will probably change quite a bit in the near future

> A small lib built upon kotlintest that aims to adopt haskell Quickcheck's functionality

## Motivation

Kotlintest already provides some means of property based testing in a data-type called `Gen<A>`. This datatype is responsible for creating random data and shrinking it. It also allows for quite a few combinators. However here lies the problem with kotlintests `Gen<A>` class. When building up more complex generators it tends to forget/ignore details of it's simpler parts. Most notably it almost always forgets its shrinker! This removes a huge advantage property tests can otherwise provide. Hence this library was modeled to conserve those properties while also providing lawful combinators.

## Usage:

> Add dependency example here when I finally get around to publishing this somewhere :)

As this lib is a port of some of haskell quickcheck's features it also features a very similar api. 

The main focus is on the typeclass `Arbitrary<A>` that defines how to generate and shrink data. Its minimal complete definition is given with the definition of `fun arbitrary(): Gen<A>` which returns a data-type `Gen<A>` (not to be confused with kotlintests `Gen<A>`).
There are several helper methods to easily build `Gen<A>` instances:

```kotlin
// Generate random elements from a predefined list
val myGen: Gen<String> = Gen.elements("1", "2", "3")

// Generate random elements from a predefined list but weighted
val myGen2: Gen<String> = Gen.frequency(
    3 toT "Hello",
    2 toT "there",
    1 toT "!"
)
```

`Gen<A>` also provides monad/applicative/functor methods through implementing arrow's `Monad`/`Applicative`/`Functor`.
This allows for combinators like:
```kotlin
val gen: Gen<Pair<Int, String>> = Gen.applicative().map(
    Gen.elements(1,3,4,5),
    Gen.frequency(3 toT "Hello", 1 toT "World")
) { (a, b) ->
   Pair(a, b) 
}
```

And many more left to be documented later ...

## Shrinkers
When building `Arbitrary` instances another method that can be implemented is `shrink(fail: A): Sequence<A>`. Given this method a failed test can try to infer a minimal example for why it failed. However building shrinkers from hand is quite a lot of boilerplate and can be very complex. That is why this lib provides combinators akin to quickcheck to build shrinker functions.

```kotlin
data class User(val name: String, val age: Int)

// with shrinkmap one can reuse an existing shrinker for an almost isomorphic type (like Tuple for data classes)
val shrinkUser: (User) -> Sequence<User> =
    shrinkMap({ u: User ->
      Tuple2(u.name, u.age)
    }, { tup: Tuple2<String, Int> ->
      User(tup.a, tup.b)
    }, Tuple2.arbitrary(String.arbitrary(), Int.arbitrary()))
```

This is quite a bit of boilerplate but that can be reduced considerably using a few different methods:

```kotlin
// use arrow-generic's @product to generate to and from tuple methods
@product
data class User(val name: String, val age: Int) {
    companion object {}
}

val shrinkUser: (User) -> Sequence<User> =
    shrinkMap({ u: User ->
        u.tupled()
    }, { tup ->
        tup.toUser()
    }, Tuple2.arbitrary(String.arbitrary(), Int.arbitrary()))
```

Using custom toTuple and fromTuple methods because the arrow ones mess with type inference,
```kotlin
// use arrow-generic's @product to generate to and from tuple methods
@product
data class User(val name: String, val age: Int) {
    companion object {
        fun toTuple(u: User) = u.tupled()
        fun fromTuple(t: Tuple2<String, Int>) = t.toUser()
    }
}

val shrinkUser: (User) -> Sequence<User> =
    shrinkMap(
        ::toTuple,
        ::fromTuple,
        Tuple2.arbitrary(String.arbitrary(), Int.arbitrary()))
```

Now when you need a full `Arbitrary<User>` instance`there is an even easier way:
```kotlin
// use arrow-generic's @product to generate to and from tuple methods
@product
data class User(val name: String, val age: Int) {
    companion object {
        fun toTuple(u: User) = u.tupled()
        fun fromTuple(t: Tuple2<String, Int>) = t.toUser()
        
        fun arbitrary(): Arbitrary<User> =
            fromTup(
                ::toTuple,
                ::fromTuple
            )
    }
}
```
> However be aware that this only works if the tuple is build from standart values (like int, string etc)
> If you need a specific type there simply pass a TupleN arbitrary instance as a third parameter.

Building `Arbitrary` instances this way ensures they will have some sort of shrinker and a lawful instance of `Gen` that can be used to build more complex instances.

## Actually running these

For now running tests with an `Arbitrary<A>` instance means converting it back to kotlintests `Gen<A>` using `.toKotlinTestGen()`. This has some implications:
* Shrinking in kotlintest continues untill all cases have been tested (unlike quickcheck that checks till a specific threshold). That means one has to be careful with recursive shrinking and excessive shrinking in general. (This will change once this lib provides its own matchers!)
* To avoid above only a subset of possible shrinking is performed with this conversation. This should still be sufficient in most cases

## Future plans

Writing own matchers that don't suffer from the limitations outlined above.

## Intances provided

Instances for `Arbitrary` are provided for:
```kotlin
Int
Long
Float
Double
Char
List<A> (ListK<A>)
Set<A> (SetK<A>)
Map<K, V> (MapK<K, V>)
String (Mapped from List<Char>)

Arrow types:
Either<L, R>
Id<A>
Ior<L, R>
NonEmptyList<A>
Option<A>
TupleN (Tuple2 up to Tuple21)
Validated<E, A>
Const<A, F>
```