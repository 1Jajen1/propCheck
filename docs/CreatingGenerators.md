# Creating data-generators and shrinkers

## Table of contents
* [Overview](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingGenerators.md#overview)
* Generators
    * [Generator size](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingGenerators.md#generator-size)
    * [Primitive generators](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingGenerators.md#primitive-generators)
    * [Combinators for Gen](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingGenerators.md#combinators-for-gen)
* [Shrinking](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingGenerators.md#shrinking)
    * [Existing shrinking functions](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingGenerators.md#existing-shrinking-functions)
* [Arbitrary typeclass instance lookups](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingGenerators.md#arbitrary-typeclass-lookups)

### Overview

Good generation and shrinking of data is one of the most important parts of property-based testing.
propCheck offers a lot of combinators to easily create both generators and shrink-functions.

On top of that `Gen<A>` implements a few important typeclasses from arrow that are essential to composing generators.

Generally what we want to do is implement the `Arbitrary` typeclass for easier use in `forAll`.

`Arbitrary` contains two methods:
- `arbitrary(): Gen<A>`
- `shrink(fail: A): Sequence<A> = emptySequence()`

Since `shrink` has a default implementation we can get away with only implementing `arbitrary`, at the cost of having no shrinking.

A full example might look like this:
```kotlin
data class User(val name: String, val age: Int, val id: Long) { 
    companion object {
        fun arbitrary(): Arbitrary<User> = object: Arbitrary<User> {
            override fun arbitrary(): Gen<User> =
                Gen.applicative().map(
                    arbitraryUnicodeString(),
                    arbitrarySizedInt(),
                    arbitraryBoundedLong()
                ) { (n, a, i) -> User(n, a, i) }.fix()

            override fun shrink(fail: User): Sequence<User> =
                Tuple3.arbitrary(String.arbitrary(), Int.arbitrary(), Long.arbitrary()).shrink(Tuple3(fail.name, fail.age, fail.id))
                    .map { (n, a, i) -> User(n, a, i) }
        }
    }
}
```

If you are using arrow-meta:
```kotlin
data class User(val name: String, val age: Int, val id: Long) { companion object }

@extension
interface UserArbitrary : Arbitrary<User> {
    override fun arbitrary(): Gen<User> =
        Gen.applicative().map(
            arbitraryUnicodeString(),
            arbitrarySizedInt(),
            arbitraryBoundedLong()
        ) { (n, a, i) -> User(n, a, i) }.fix()

    override fun shrink(fail: User): Sequence<User> =
        Tuple3.arbitrary(String.arbitrary(), Int.arbitrary(), Long.arbitrary()).shrink(Tuple3(fail.name, fail.age, fail.id))
            .map { (n, a, i) -> User(n, a, i) }
}
```

### Generator size
A generator is, in short, a function from a random seed and a size to a value (`(Seed) -> (Size) -> Value`). The intuition here is that size grows larger the more data we generate, this is to enable starting with small data and going larger and larger if needed. This ensures tests fail fast if the size of data is not relevant to the test.

The size parameter can be accessed to, for example, build recursive generators with a fixed increasing depth.

### Primitive generators

There are a number of generators already included in propCheck. A quick list:
- `arbitrarySizedInt/Byte/Long` provides a generator for numbers delimited by the size parameter
- `arbitraryBoundedInt/Byte/Long` provides a generator for numbers from their entire range
- `arbitrarySizedFloat/Double` provides a generator for floating point numbers delimeted by the size parameter
- `arbitraryBoundedFloat/Double` provides a generator for floating point numbers from their entire range
- `arbitraryASCIIChar` generator for ascii chars
- `arbitraryUnicodeChar` generator for unicode chars
- `arbitraryASCIIString` generator for ascii strings
- `arbitraryUnicodeString` generator for unicode strings

### Combinators for Gen

####`Gen.applicative()`

If you are already familiar with Applicatives just skip this.
> This is by no means an explanation for Applicative, just a tutorial to use it with Gen.

Using `Gen.applicative().map` in essence just combines the generators passed to map to return a tuple and gives us the tuple in a function to create our result from all the values inside.

Use this whenever your datatype is isomophic to a tuple. (There are also generated methods fromTup to do the same thing, but intelliJ hates them with passion, they will slow down your ide...)

####`Gen.monad()`

Same as above, when you are already familiar with monads just skip this.
> This is no monad tutorial, that is a bit beyond the scope of this, and also not needed to just use it.

Using `Gen.applicative` enables us to compose generators in an independent way, but it does not help us if the we try to do something like `Gen.map { anotherGen }` or `Gen.applicative().map(a, b) { anotherGen }`. Using those methods will just create nested generators which are not usable at all.
This is a frequent pattern when writing generators that depend on the current size parameter (more on this [here](TODO link)) or with recursive generators.

In essence `Gen.monad().run { .. }` gives us access to `Gen<A>.flatMap(f: (A) -> Gen<B>): Gen<B>` which solves the above problem.

However if these flatMap calls end up being nested things get unwieldy fast, so arrow comprehensions to the rescue:

Using just flatMap (on a not very useful example):
```kotlin
Gen.monad().run { 
    arbitrarySizedInt().flatMap { i -> arbitrarySizedInt().flatMap { j -> arbitraryUnicodeString().map { it.length in i..j } } }
}
```

Using arrow's comprehensions:
```kotlin
Gen.monad().fx.monad {
    val i = !arbitrarySizedInt()
    val j = !arbitrarySizedInt()
    arbitraryUnicodeString().map { it.length in i..j }
}
```

> If you want to learn more about how this works and how you can benefit from this in other data-types as well check out arrow-kt's documentation, it is really good.

#### `Gen<A>.map(f: (A) -> B): Gen<B>`

Map over the value generated.

```kotlin
arbitraryBoundedByte().map { it.toInt() }
```

#### `Gen<A>.resize(i: Int): Gen<A>`


Set the size parameter to a given value.

#### `Gen<A>.scale(f: (Int) -> Int): Gen<A>`


Apply a function to the size parameter


#### `<A> sized(f: (Int) -> Gen<A>): Gen<A>`

Build a generator with access to the size parameter.

#### `getSize(): Gen<Int>`

Get a generator "generating" its size parameter.

#### `Gen<A>.suchThat(f: (A) -> Boolean): Gen<A>`

Filter results of a generator.
> Warning: This is not stack-safe, do not fail too many times if possible

#### `Gen<A>.suchThatOption(f: (A) -> Boolean): Gen<Option<A>>`

Filter results of a generator.
> Warning: This is not stack-safe, do not fail too many times if possible

#### `Gen<A>.suchThatMap(f: (A) -> Option<B>): Gen<B>`

Map and filter results of a generator
> Warning: This is not stack-safe, do not fail too many times if possible

#### `Gen<A>.listOf(): Gen<List<A>>`

Generate a list of `A` with length based on the size parameter.

#### `Gen<A>.nelOf(): Gen<Nel<A>>`

Generate a non-empty list of `A` with length based on the size parameter.

#### `Gen<A>.vectorOf(n: Int): Gen<List<A>>`

Generate a list of `A` with a fixed length.

#### `<A>choose(range: Tuple2<A, A>, randA: Random<A>): Gen<A>`

Choose random values from within a range.
> `Random<A>` is a custom typeclass built around random generation of data. This is mostly used for primitives and as such already exists for most basic types.

#### `<A>chooseAny(randA: Random<A>): Gen<A>`

Choose random values from the entire range of `A`.
> `Random<A>` is a custom typeclass built around random generation of data. This is mostly used for primitives and as such already exists for most basic types.

#### `<A>oneOf(vararg gens: Gen<out A>): Gen<A>`

Chooses randomly between the passed generators.
>Throws when given no generators.

#### `<A>frequency(vararg gens: Tuple2<Int, Gen<out A>>): Gen<A>`

Chooses randomly, but weighed between the passed generators.
>Throws when given no generators

#### `<A>elements(vararg els: A): Gen<A>`

Chooses randomly between the elements supplied as arguments.
>Throws when given no elements

#### `<A>sublist(l: List<A>): Gen<List<A>>`

Generates lists containing a subset of the values of the supplied list.

#### `<A>shuffle(l: List<A>): Gen<List<A>>`

Generates random permutations of a list.

#### `<A: Enum<A>>.fromEnum(): Gen<A>`

Generates random values from all possible enum values.
>`fromEnum() = elements(*enumValues())`

---
### Shrinking

Property based testing without shrinking is only half the fun. Shrinking will try to convert a failing test case into a minimal test case, making understanding the issue much much easier.
Shrinking is based on the assumption that smaller input data leads to smaller test cases. It works by retrying the property with a number of smaller inputs, restarting that process when it finds a new failing case.

Implementing the shrinking function is best done by using `shrinkMap` and using an existing shrinking function.

```kotlin
shrinkMap({ 
    // to other
    Tuple3(it.name, it.age, it.id)
}, { (n, a, id) ->
    // from other
    User(n, a, id)
}, Tuple3.arbitrary(String.arbitrary(), Int.arbitrary(), Long.arbitrary()))
```

### Existing shrinking functions

If we need to implement shrinking on our own, it's best to use existing functions as building blocks. The following are a few shrinkers for "primitive-like" types:
- `shrinkList` shrink a list to smaller list and/or shrink the elements themselves
- `shrinkByte/Int/Long` shrink numbers
- `shrinkFloat/Double` shrink floating point numbers
- `shrinkChar` shrink characters
- `shrinkMap` map to a value that already has an arbitrary instance to shrink it

### Arbitrary typeclass instance lookups

Currently we support looking up a few instances for `Arbitrary` through reflection using `defArbitrary()`.
>(That will be deprecated when the arrow plugin hits)

Supported instances:

Primitve-like types:
* `Int` -> `Int.arbitrary()` for the `Arbitrary<Int>` instance (uses sizedInt)
* `Long` -> Same as above. Just substitute Int for Long
* `Byte` -> Same as above. Just substitute Int for Byte
* `Float` -> `Float.arbitrary()` (uses sizedFloat)
* `Double` -> Same as above. Just substitute Float for Double
* `Char` -> `Char.arbitrary()` (uses ascii atm, might change later)
* `String` -> `String.arbitrary()` (uses ASCII atm, might change later)
* `Boolean` -> `Boolean.arbitrary()`
* `IntArray` -> `intArrayArb`
* `LongArray` -> `longArrayArb`
* `FloatArray` -> `floatArrayArb`
* `DoubleArray` -> `doubleArrayArb`
* `ByteArray` -> `byteArrayArb`
* `BooleanArray` -> `booleanArrayArb`
* `Array<T>` -> `arrayArb()` // Cannot be infered by `defArbitrary` atm
* `(A) -> B` // Generate using the `Fun<A, B>` wrapper, will be documented better later on. (Requires an instance of `Func<A>`, `Coarbitrary<A>` and `Arbitrary<B>`)

Collections:
The `K` variants are `arrow` wrappers. They are isomorphic to their non-k variants and can be used as such. That is to say: Every `ListK` is a `List` and vice versa.
* `List<A>` -> `ListK.arbitrary()`
* `Set<A>` -> `SetK.arbitrary()`
* `Map<A>` -> `MapK.arbitrary()`

Pair, Triple and TupleN;
There are arbitrary instances for Pair and Triple, but they are not yet present as extension methods.
In general TupleN is more powerful anyway.
* `TupleN` -> `Tuple2.arbitrary()` // replace 2 with up to 22 to get different tuple sizes

Arrow datatypes:
* `Either<L, R>`
* `Id<A>`
* `Ior<L, R>`
* `NonEmptyList<A>`
* `Option<A>`
* `Validated<E, A>`

Test helper types:
These types include some general functionality to avoid having to write new instances for them.
* `Blind<A>` // `A` but it's show instance does not print `A`. Useful to hide messy data
* `Fixed<A>` // `A` but does not perform shrinking.
* `OrderedList<A>` // outputs sorted lists of `A`. Can not be infered by `defArbitrary`
* `Smart<A>` // `A` but tries a different order when shrinking
* `Shrink2<A>` // `A` but while shrinking, shrinks twice
* `Shrinking<S, A>` // `A` but keeps a state `S` while shrinking
* `Positive<A: Number>` // Only numbers > 0. (Only works on Numbers and can only infer the ones noted above)
* `NonNegative<A: Number>` // Only numbers >= 0. Same limitations as positive
* `Negative<A: Number>` // Only numbers < 0. Same limitations as positive
* `NonPositive<A: Number>` // Only numbers <= 0. Same limitations as positive.
