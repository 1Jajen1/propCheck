# propCheck - Property based testing in kotlin

> A small library built upon kotlintest that aims to adopt haskell [Quickcheck's](https://github.com/nick8325/quickcheck) functionality.
It started out as a straight port of quickcheck and was adapted slightly to work better with kotlin

## Usage

Add the following to your `build.gradle`:
```groovy
repositories {
    maven { url 'https://dl.bintray.com/jannis/propCheck-kt' }
}

dependencies {
    testImplementation: 'propCheck:propCheck-kt:0.9'
}
```

Example usage:
```kotlin
propCheck {
    forAll { (a, b): Pair<Int, Int> ->
        a + b == b + a
    }
}
// prints =>
+++ OK, passed 100 tests.
```

In this example propCheck ran 100 tests with random pairs of integers and checks if addition over integers is commutative.

`propCheck` can also take an argument of type `Args` to change the way tests are run:
```kotlin
propCheck(Args(maxSuccess = 300)) {
    forAll { (a, b): Pair<Int, Int> ->
        a + b == b + a
    }
}
// prints =>
+++ OK, passed 300 tests.
```

A full overview of what can be customised can be seen [here](https://github.com/1Jajen1/propCheck/blob/master/README.md#args)

## Shrinking

A powerful concept in property based testing is shrinking. Given a failed test a shrinking function can output several "smaller" examples.
For example: Lists tend to shrink to smaller lists, numbers shrink towards zero and so on.
This often ends you with a minimal counterexample for the failed test and is very useful if the data would otherwise be messy (as will likely happen with random data).

For most cases enabling shrinking is as easy as changing [forAll](https://github.com/1Jajen1/propCheck#forall) to [forAllShrink](https://github.com/1Jajen1/propCheck#forallshrink):
```kotlin
propCheck { 
    forAllShrink { i: Int ->
        i < 20
    }
}
// prints =>
*** Failed! (after 35 tests and 3 shrinks):
Falsifiable
20
```

## Testing custom classes

To understand how to generate custom data for testing it is best to look at the `Arbitrary<A>` interface first.
`Arbitrary` defines two methods: 
```kotlin
fun arbitrary(): Gen<A>
fun shrink(fail: A): Sequence<A> = emptySequence()
```
Since shrink has a default implementation (no shrinking) the main focus for generating custom data is on `.arbitrary()`.

In the following examplee we wirite a simple generator for a user data class:
```kotlin
data class User(val name: String, val age: Int, val friends: List<String>)

val userArb = object : Arbitrary<User> {
    override fun arbitrary(): Gen<User> = Gen.applicative().map(
        arbitraryASCIIString(),
        arbitrarySizedInt(),
        ListK.arbitrary(String.arbitrary()).arbitrary()
    ) { (name, age, friends) ->
        User(name, age, friends)
    }.fix()
}
```
Here we combine three generators (String, Int, List<String>) and map their result to a user.

Let's add a way to shrink this user:
```kotlin
val userArb = object : Arbitrary<User> {
    override fun arbitrary(): Gen<User> = ...
    override fun shrink(fail: User): Sequence<User> =
        override fun shrink(fail: User): Sequence<User> =
                    shrinkMap({ user ->
                        Tuple3(user.name, user.age, user.friends.k())
                    }, { (name, age, friends) ->
                        User(name, age, friends)
                    }, Tuple3.arbitrary(
                         String.arbitrary(),
                         Int.arbitrary(),
                         ListK.arbitrary(String.arbitrary())
                       )
                    ).invoke(fail)
}
```
Here `shrinkMap` is used to implement shrinking. `shrinkMap` returns a shrinking function for a type that can be converted from `A` to `B` and back by using an existing instance of `Arbitrary<B>`. Since data classes in general can be expressed as tuples and `Tuple3` already has an arbitrary instance (with shrinking) we can take advantage of that.

This is how a failed test would look with shrinking (just output, no exceptions)
```kotlin
propCcheck {
    forAllShrink(userArb) { user ->
        user.age < 20 // Fail for users older than 20
    }
}
// output =>
*** Failed! (after 32 tests and 3 shrinks):
Falsifiable
User(name=, age=20, friends=ListK(list=[]))
```
Our shrinking worked and we are left with a minimal example.

Now there are several ways of simplifying all of this:
* use `fromTup` which given a function from and to a TupleN returns an Arbitrary instance with shrinking
* use [arrow-generic](https://arrow-kt.io/docs/generic/product/) to auto generate from and to tuple functions
* use `defArbitrary` which can for most `A`'s infer a Arbitrary instance. A full list can be seen [here](https://github.com/1Jajen1/propCheck/blob/master/README.md#types-with-default-implementations).

This is as concise as it can get: (Given functions for to and from tup are defined or generated with arrow)
```kotlin
val userArbitrary: Arbitrary<User> = fromTup(::toTuple, ::fromTuple)
```

### The `Gen<A>` datatype

At the heart of generating testing data is the `Gen<A>` datatype. It represents a function from `(Long) -> (Int) -> A`, which translates to: Given a random seed and a size parameter it returns an A.

Hand crafting `Gen` instances is made very easy with powerful combinators.
In this following example we create a generator for sorted non-empty lists with integers:
```kotlin
val sortedNelGen: Gen<List<Int>> = arbitrarySizedInt()
    .listOf()
    .suchThat { it.isNotEmpty() }
    .map { it.sorted() }
```
> `arbitrarySizedInt` returns a generator for ints that depends on the size parameter.

A more complex example could be generating a `BinaryTree` of ints:
```kotlin
sealed class BinaryTree {
    data class Leaf(val i: Int): BinaryTree()
    data class Branch(val left: BinaryTree, val right: BinaryTree): BinaryTree()
}

fun binaryTreeGen(): Gen<BinaryTree> = Gen.sized { size ->
    if (size == 1) arbitrarySizedInt().map { BinaryTree.Leaf(it) as BinaryTree }
    else Gen.frequency(
        1 toT arbitrarySizedInt().map { BinaryTree.Leaf(it) as BinaryTree },
        3 toT binding { BinaryTree.Branch(binaryTreeGen().resize(size - 1).bind(), binaryTreeGen().resize(size - 1).bind()) as BinaryTree }.fix()
    )
}
```
This example generates binary-trees based with a maximum depth based on the size parameter. Using `Gen.frequency` the chance of getting a `Branch` instead of a `Leaf` is adjusted.
> This is using the amazing fp library [arrow](https://arrow-kt.io/) for `binding`. This is not necessary, it will however ease creation of nested `Gen`s and lots of other more complex generators.

## A note regarding test data
The quality of a property-based test is directly related to the quality of the data fed to it. There are some helpers to test and assure that the generated data holds some invariants.

To inspect results use either [label](https://github.com/1Jajen1/propCheck#label), [cover](https://github.com/1Jajen1/propCheck#cover), [classify](https://github.com/1Jajen1/propCheck#classify) or [tabulate](https://github.com/1Jajen1/propCheck#tabulate).

Here is an example on how to use [classify](https://github.com/1Jajen1/propCheck#classify):
```kotlin
propCheck {
    forAll(OrderedList.arbitrary(Int.order(), Int.arbitrary())) { (l): OrderedList<Int> ->
        classify(
            l.size > 1,
            "non-trivial",
            l.shuffled().sorted() == l
        )
    }
}
// prints something like this =>
+++ OK, passed 100 tests (92,00% non-trivial).
```

To fail a test with insufficient coverage use [checkCoverage](https://github.com/1Jajen1/propCheck#checkcoverage) with functions like [cover](https://github.com/1Jajen1/propCheck#cover) or [coverTable](https://github.com/1Jajen1/propCheck#covertable).
```kotlin
propCheck {
    forAll(OrderedList.arbitrary(Int.order(), Int.arbitrary())) { (l): OrderedList<Int> ->
        checkCoverage(
            cover(
                95.0,
                l.size > 1,
                "non-trivial",
                l.shuffled().sorted() == l
            )
        )
    }
}
// prints something like this =>
*** Failed! (after 800 tests):
Insufficient coverage
89,99% non-trivial
```
> Here a coverage of 95% non-trivial lists is required, but only 89.99% could be reached.

## Running these tests with a test runner

propCheck is by itself stand-alone and does not provide test-runner capabilites like kotlintest. It however can and should be used together with a test-runner. By default `propCheck` will throw exceptions on failure and thus will cause the test case it is being run in to fail. (That can be diasbled by using methods like `propCheckWithResult` instead)

The following example runs a test in a kotlintest test:
```kotlin
class TestSpec : StringSpec({
    "test if positive is always > 0!" {
        propCheck {
            forAll { (i): Positive<Int> ->
                i > 0
            }
        }
    }
    "other tests" { ... }
})
```

## Use of arrow with propCheck

First of all: If you are using [arrow-kt](https://arrow-kt.io/) everything is good. There are plenty of instances already defined for arrows data-types.

If not then don't worry. The entire api can be used without ever touching upon arrows datatypes and there are overloads specifically to avoid those cases if they do come up.
> That is excluding Option but I don't think that will be a problem

## Kotlintest generators vs propCheck
Kotlintest already includes some means of property based testing. However their data-types and reflection-lookups are severly limited. The reason I ported quickcheck over is because it is built upon lawful instances for its datatypes and features a much richer set of methods. This makes testing with propCheck much easier and much more powerful.

> There is however a helper method to convert a `Arbitrary<A>` to a kotlintest `Gen<A>`. That approach is somewhat limited though, especially when it comes to shrinking.

## Feedback
`propCheck` is still in its early days so if you notice bugs or think something can be improved please create issues or shoot me a pull request. All feedback is highly appreciated.

## Credits
At this point I'd like to thank the [arrow-kt](https://arrow-kt.io/) creators and maintainers as this library would not have been possible without them. The same goes for [quickcheck](https://github.com/nick8325/quickcheck) which `propCheck` is based on.

---

## Api overview

### Setting up property based tests

The following combinators change how your test is run and determine the result:

#### forAll

`forAll` runs a test without shrinking a number of times (depends on the arguments => default 100)

There are several overloads for `forAll`:
```kotlin
/* The most common version of forAll. For default types (listed below)
    the arbitrary instance can be looked up with reflection
 */
inline fun <reified A, reified B : Any> forAll(
    arbB: Arbitrary<B> = defArbitrary(),
    testable: Testable<A> = defTestable(),
    showB: Show<B> = defShow(),
    noinline prop: (B) -> A
): Property

/* This is the most explicit form of forall
    because it takes and explicit generator
 */
inline fun <reified A, B> forAll(
    genB: Gen<B>,
    testable: Testable<A> = defTestable(),
    showB: Show<B> = Show.any(),
    noinline prop: (B) -> A
): Property
```

#### forAllShrink

Like `forAll` but tries to shrink failed test cases.

#### forAllBlind

Like `forAll` but does not print a counterexample on failure.

#### forAllShrinkBlind

like `forAllShrink` but does not print a counterexample on failure.

#### discardIf

Discard test data based on some premise. Can skew test data.

Usage:
```kotlin
propCheck {
    forAll { i: Int ->
        discardIf(
            i < 0, // discard values below 0
            i > 10 // test
        )
    }
}
```

Use functions like label/classify/tabulate to check if your test data is still good.

#### verbose

Generate more output while testing

```kotlin
propCheck {
    forAll { i: Int ->
        verbose(
            i < 10
        )
    }
}
// prints something like this =>
Passed: 4

Failed: 11

*** Failed! (after 2 tests):
Falsifiable
11
```

#### verboseShrinking

Like `verbose` but prints on shrink attempts as well.

#### whenFail

Performs an action after a test failed.
```kotlin
propCheck {
    forAll { i: Int ->
        whenFail({ println("We failed") }, i > 10)
    }
}
```

#### whenFailIO

Variant of `whenFail` that takes an `IO<Unit>` instead of a function.

#### whenFailEvery

Performs an action after every failed test (includes shrinking).

#### whenFailEveryIO

Variant of `whenFailEvery` that takes an `IO<Unit>` instead of a function. 

#### callback

Attach a callback to a test case.

#### label

Attach a label to a test case.
```kotlin
propCheck {
    forAll { i: Int ->
        label(
            if (i.rem(2) == 0) "evens"
            else "odds", // label
            i + i == 2 * i // test case
        )
    }
}
// prints something like this =>
+++ OK, passed 100 tests:
46,00% odds
54,00% evens
```

#### collect

Like label, but uses an implicit show function. (Basically calls toString on every testcase and uses that as its label)

#### classify

Attach a label to a test case if it holds a condition
```kotlin
propCheck {
    forAll { i: Int ->
        classify(
            i == 0, // condition
            "zero", // label
            classify(
                i != 0, // condition
                "non zero", // label
                i + i == 2 * i // test
            )
        )
    }
}
// prints something like this =>
+++ OK, passed 100 tests (97,00% non zero. 3,00% zero).
```

#### cover

Check the coverage of a specific label. Only fails a test when `checkCoverage` is used.
```kotlin
propCheck {
    forAll { i: Int ->
        checkCoverage(
            cover(
                20.0, // percentage required
                i == 0, // condition
                "zero", // label
                i + i == 2 * i // test
            )
        )
    }
}
// prints something like this =>
*** Failed! (after 400 tests):
Insufficient coverage
4,51% zero
```

#### tabulate

Label test data in differnt tables.
```kotlin
propCheck {
    forAll { i: Int ->
        tabulate(
            "Data",
            listOf(i.toString()),
            i + i == 2 * i
        )
    }
}
// prints something like this =>
+++ OK, passed 100 tests.
Data (100 in total)
3,00% -2
2,00% 2
2,00% 9
...
```

#### coverTable

Like `cover` but for labels in tables. Again no failure unless used with `checkCoverage`

#### checkCoverage

Enable coverage based checks based.

#### withMaxSuccess

Change the number of tests to run.

#### again

Continue testing if it did not fail.

#### once

Abort after this test regardless of result.

#### expectFailure

Expect the test to fail. 

#### counterexample

Provide a counterexample that will be printed on failure.

#### noShrinking

Disable shrinking.

#### shrinking

Enable shrinking.

#### mapSize

Change the current size parameter used.

#### mapTotalResult

Map over the current test result. Must be a total function. (Should never throw)

#### liftBoolean

Lift a boolean to a test result.
```kotlin
true -> test succeeds
false -> failure
```

#### ioProperty

Run a test in `ÌO<A>`. Will not use shrinking.

#### idempotentIOProperty

Run a test in `IO<A>` with shrinking. Might rerun the `IO` during shrinking!

---

### Generator combinators

All combinators and utility functions for creating generators

`Gen` also implements `Monad` which means it has access to `arrow`s binding syntax and some useful methods.
#### `Gen<A>.map(f: (A) -> B): Gen<B>`

Map over the generated value.
```kotlin
val badGenLong: Gen<Long> = arbitrarySizedInt().map { it.toLong() }
```

#### `Gen<A>.resize(n: Int): Gen<A>`

Change the size parameter to the supplied value. Must be >= 0

```kotlin
// this will generate 10 every time
val resizedGen: Gen<Int> = Gen.getSize().resize(10)
```

#### `Gen<A>.scale(f: (Int) -> Int): Gen<A>`

Scale a generators size parameter with a function.

#### `Gen<A>.suchThat(f: (A) -> Boolean): Gen<A>`

Filter generated values. This will retry until it is successful, increasing the size parameter.
> It can currently overflow the stack if it continues too long. If possible this will be fixed.

#### `Gen<A>.suchThatOption(f: (A) -> Boolean): Gen<Option<A>>`

Filter generated values. This will retry a few times and then return `None` if it fails to generate a value.
> As with suchThat this can overflow the stack if it takes too many iterations

#### `Gen<A>.suchThatMap(f: (A) -> Option<B>): Gen<B>`

Maps to a value of type `Option<B>` and filters out all `None` cases.

#### `Gen<A>.listOf(): Gen<List<A>>`

Create a generator outputting a list of `A`s. Size depends on the size parameter.

#### `Gen<A>.nelOf(): Gen<Nel<A>>`

Create a generator generating a non-empty list of `A`s. Size depends on size parameter.

#### `Gen<A>.vectorOf(n): Gen<List<A>>`

Create a generator generating a list of `A`s of size n

#### `Gen.sized(f: (Int) -> Gen<A>): Gen<A>`

Create a generator that has access to the size parameter

#### `Gen.getSize(): Gen<Int>`

Create a generator that returns its size parameter

#### `Gen.choose(range: Tuple2<A, A>, randA: Random<A>): Gen<A>`

Generate random values between a range. `Random<A>` is just an interface that defines methods to generate random data of type `A`.
Default instances for primitve types are included.

#### `Gen.chooseAny(randA: Random<A>): Gen<A>`

Generate random values from `A`s entire range.

#### `Gen.oneOf(vararg gens: Gen<A>): Gen<A>`

Choose between multiple generators. `gens` must be greater than 0.

#### `Gen.frequency(vararg gens: Tuple2<Int, Gen<A>>): Gen<A>`

Choose between multiple generators but factor in weight for each one. `gens` must be greater than 0.

#### `Gen.elements(vararg as: A): Gen<A>`

Choose between a provided set of values. `as` must be greater than 0.

#### `Gen.sublistOf(l: List<A>): Gen<List<A>>`

Generate random sublists of `l`.

#### `Gen.shuffle(l: List<A>): Gen<List<A>>`

Generate different permutations of l.

---

## Types with default implementations

This is a list of types that have predefined instances for `Arbitrary` and thus their instances must not be supplied to functions like `forAll` unless explicitly noted.

### Arbitrary instances and generators

#### Primitve-like types
* `Int` -> `Int.arbitrary()` for the `Arbitrary<Int>` instance, (`arbitrarySizedInt`, `arbitrarySizedPositiveInt`, `arbitraryBoundedInt` for generators)
* `Long` -> Same as above. Just substitute Int for Long
* `Byte` -> Same as above. Just substitute Int for Byte
* `Float` -> `Float.arbitrary()`, `arbitrarySizedFloat`
* `Double` -> Same as above. Just substitute Float for Double
* `Char` -> `Char.arbitrary()`, `arbitraryASCIIChar`, `arbitraryUnicodeChar`
* `String` -> `String.arbitrary()`, `arbitraryASCIIString`, `arbitraryUnicodeString`
* `Boolean` -> `Boolean.arbitrary()`
* `IntArray` -> `intArrayArb`
* `LongArray` -> `longArrayArb`
* `FloatArray` -> `floatArrayArb`
* `DoubleArray` -> `doubleArrayArb`
* `ByteArray` -> `byteArrayArb`
* `BooleanArray` -> `booleanArrayArb`
* `Array<T>` -> `arrayArb()` // Cannot be infered by `defArbitrary` atm

#### Collections
The `K` variants are `arrow` wrappers. They are isomorphic to their non-k variants and can be used as such. That is to say: Every `ListK` is a `List`
* `List<A>` -> `ListK.arbitrary()`
* `Set<A>` -> `SetK.arbitrary()`
* `Map<A>` -> `MapK.arbitrary()`

#### Pair, Triple and TupleN
There are arbitrary instances for Pair and Triple, but they are not yet present as extension methods.
In general TupleN is more powerful anyway.
* `TupleN` -> `Tuple2.arbitrary()` // replace 2 with up to 22 to get different tuple sizes

#### Arrow datatypes
* `Either<L, R>`
* `Id<A>`
* `Ior<L, R>`
* `NonEmptyList<A>`
* `Option<A>`
* `Validated<E, A>`

#### Test helper types
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

---

### Args

`Args` represents the arguments that can be passed to `propCheck`.

Possible fields:
* `replay: Option<Tuple2<Long, Int>>` -> if specified uses the given seed and size parameter
* `maxSuccess: Int` -> Number of tests to run
* `maxDiscardRatio: Int` -> Ratio that if exceeded gives up testing
* `maxSize: Int` -> The maximum size that is passed to a generator
* `verbose: Boolean` -> More output.
* `maxShrinks: Int` -> Maximum shrinking attempts performed