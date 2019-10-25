# Introduction to using propCheck

> This is not an introduction for property-based testing. For that look at this [blog](https://fsharpforfunandprofit.com/posts/property-based-testing/) (F#), watch any [talk](https://www.youtube.com/watch?v=hXnS_Xjwk2Y) by John Hughes or in general just look online, there are many good resources.

## Table of contents

* [Basic Usage](https://github.com/1Jajen1/propCheck/blob/master/docs/Introdution.md#basic-usage)
* [Testing custom datatypes](https://github.com/1Jajen1/propCheck/blob/master/docs/Introdution.md#testing-custom-datatypes)
* [Testing IO](https://github.com/1Jajen1/propCheck/blob/master/docs/Introdution.md#testing-io)
* [Args](https://github.com/1Jajen1/propCheck/blob/master/docs/Introdution.md#args)

### Basic Usage

Let's break down an example:
```kotlin
propCheck {
    forAll { (a, b): Pair<Int, Int> ->
        a + b == b + a
    }
}
// prints =>
+++ OK, passed 100 tests.
```

```kotlin
propCheck { .. }
```
Every test starts with a call to this (or one of it's variants) function. It has an optional argument for arguments (like a different random seed, etc) and a required argument that is of type `() -> Property` (This being a function is for pure convenience).
> `Property` is a datatype that describes something that has been tested (yes "has been"). For example `Boolean`, `TestResult` all implement the `Testable<A>` typeclass, providing means to convert to the `Property` datatype, but that is rarely done manually.

```kotlin
forAll { (a, b): Pair<Int, Int> -> ... }
```
`forAll` is one of the most common ways to create a `Property`. It has a few overloads, but if you are not using custom data-types you can use the default version.
> Check [here](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingProperties.md#forall) for an in-depth overview of forAll and it's variants

The intuition for `forAll` is that it tests the inner property with "all" parameters. In practice only a subset of possible values is tested, but that is usually enough.
> If not check out verifying coverage [here](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingProperties.md#inspecting-test-data). 

```kotlin
a + b == b + a
```
This is finally the actual property that we are testing. In this case commutativity of pairs of integers.

Unless something much more complex is needed, this structure will be the most common way of testing properties.

### Testing custom datatypes

While the above is all well and fine, most data is not just standard datatypes, and getting `forAll` to generate custom types is a bit more work:

To generate data `forAll` uses instances of `Gen<A>` (which can be implicit for standard types), and to shrink data (more on that later) a function `(A) -> Sequence<A>`. This can either be supplied on its own or through an instance of the `Arbitrary<A>` typeclass. Since the later is easier to use we'll look at that in more detail:

`Arbitrary` has two methods associated with it:
```kotlin
fun arbitrary(): Gen<A>
fun shrink(fail: A): Sequence<A> = emptySequence()
```
>For the beginning we will ignore shrinking and just use the default (no shrinking) there.

Below you can see an example of implementing `Arbitrary` for a simple user data class.
```kotlin
data class User(val name: String, val age: Int, val friends: List<String>)

val userArb: Arbitrary<User> = Arbitrary(
    Gen.applicative().map(
        arbitraryASCIIString(),
        arbitrarySizedInt(),
        ListK.arbitrary(String.arbitrary()).arbitrary()
    ) { (name, age, friends) ->
        User(name, age, friends)
    }.fix()
)
```
Quite a bit to take in, so let's break it down:

`Arbitrary(..)` is an invoke constructor that when supplied with a `Gen<A>` returns an `Arbitrary<A>`.

`Gen.applicative().map(Gen<A>, Gen<B>) { (a, b) -> .. }` This scary looking method comes from arrow and, in short, combines a number of `Gen<*>` to one single `Gen<A>`. (There is much more to `Applicative`, but for this example that understanding is enough).
In this case we are combining `arbitraryASCIIString(): Gen<String>`, `arbitrarySizedInt(): Gen<Int>` and `ListK.arbitrary(String.arbitrary()): Gen<ListK<String>>` and mapping the result of these three to `Gen<User>`.
> If you want the result of a `Gen<A>` to depend on a result of another `Gen<B>` you need a different method than `applicative().map` but more on that [here](TODO link)

### Testing IO

Quite a bit of code has side-effects, testing it within propCheck should use the `IO` wrapper type from arrow (or suspend functions). This is for a number of reasons:
- Side-effect code can throw errors at any time and propCheck without `IO` assums no runtime errors (for simplicity mainly). This may change, but for now, you need to wrap error throwing code (either in IO, or catch errors yourself)
- Side-effect code can be non-deterministic. The test itself can and will be run multiple times, and for accurate results and good shrinking deterministic code is king. If you really need side-effects you will need to give up on a few of these advantages.

So how does one actually test IO/suspend functions:

- One way is to use the `ioProperty` combinator:
```kotlin
fun doSideEffectsWithLong(l: Long): IO<Boolean> = IO { throw Throwable("Side effects are bad") }
propCheck {
    forAll { l: Long ->
        ioProperty(
            doSideEffectsWithLong(l)
        )
    }
}
*** Failed! (after 1 test):
Exception
0
```
> This will never shrink failure cases.

- Another way (if you side-effect is idempotent) is using the `idempotentIOproperty`:
```kotlin
fun doSideEffectsWithString(l: Long): IO<Boolean> = IO { l < 20 || throw Throwable("Side effects are bad") }
propCheck {
    forAllShrink { l: Long ->
        idempotentIOProperty(
            doSideEffectsWithString(l)
        )
    }
}
*** Failed! (after 24 tests and 1 shrink):
Exception
20
```
> This will shrink, but in order to do so it needs to re-execute the IO over and over again.

In general it is best to have as few `IO` tests as possible.

### Args

`Args` represents the arguments that can be passed to `propCheck`.

Possible fields:
* `replay: Option<Tuple2<Long, Int>>` -> if specified uses the given seed and size parameter
* `maxSuccess: Int` -> Number of tests to run
* `maxDiscardRatio: Int` -> Ratio that if exceeded gives up testing
* `maxSize: Int` -> The maximum size that is passed to a generator
* `verbose: Boolean` -> More output.
* `maxShrinks: Int` -> Maximum shrinking attempts performed
