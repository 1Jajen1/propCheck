# propCheck - Property based testing in kotlin

[![CircleCI](https://circleci.com/gh/1Jajen1/propCheck/tree/master.svg?style=svg)](https://circleci.com/gh/1Jajen1/propCheck/tree/master)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Download](https://api.bintray.com/packages/jannis/propCheck-kt/propCheck-kt/images/download.svg) ](https://bintray.com/jannis/propCheck-kt/propCheck-kt/_latestVersion)

> A small library built upon kotlintest that aims to adopt haskell [Quickcheck's](https://github.com/nick8325/quickcheck) functionality.
It started out as a straight port of quickcheck and was adapted slightly to work better with kotlin

## Table of contents

* [Documentation](https://github.com/1Jajen1/propCheck#documentation)
* [Usage](https://github.com/1Jajen1/propCheck#usage)
* [Shrinking](https://github.com/1Jajen1/propCheck#shrinking)
* [A note regarding test data](https://github.com/1Jajen1/propCheck#a-note-regarding-test-data)
* [Running with a test runner](https://github.com/1Jajen1/propCheck#running-these-tests-with-a-test-runner)
* [Use of arrow in and with propCheck](https://github.com/1Jajen1/propCheck#use-of-arrow-in-and-with-propcheck)
* [Kotlintest generators vs propCheck](https://github.com/1Jajen1/propCheck#kotlintest-generators-vs-propcheck)
* [Feedback](https://github.com/1Jajen1/propCheck#feedback)
* [Credits](https://github.com/1Jajen1/propCheck#credits)

## Documentation
* [Introduction](https://github.com/1Jajen1/propCheck/blob/master/docs/Introduction.md)
* [Creating Generators](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingGenerators.md)
* [Creating Properties](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingProperties.md)
* [Generating functions](https://github.com/1Jajen1/propCheck/blob/master/docs/GeneratingFunctions.md)
* [State-machine-testing and testing for race-conditons](https://github.com/1Jajen1/propCheck/blob/master/docs/StatemachineTesting.md)
   
## Usage

Add the following to your `build.gradle`:
```groovy
repositories {
    maven { url 'https://dl.bintray.com/jannis/propCheck-kt' }
}

dependencies {
    testImplementation: 'propCheck:propCheck-kt:0.9.6'
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

A full overview of what can be customised can be seen [here](https://github.com/1Jajen1/propCheck/blob/master/docs/Introduction.md#args)

## Shrinking

A powerful concept in property based testing is shrinking. Given a failed test a shrinking function can output several "smaller" examples.
For example: Lists tend to shrink to smaller lists, numbers shrink towards zero and so on.
This often ends you with a minimal counterexample for the failed test and is very useful if the data would otherwise be messy (as will likely happen with random data).

For most cases enabling shrinking is as easy as changing [forAll](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingProperties.md#forall) to [forAllShrink](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingProperties.md#forallshrink):
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

## A note regarding test data
The quality of a property-based test is directly related to the quality of the data fed to it. There are some helpers to test and assure that the generated data holds some invariants.

To inspect results use either [label](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingProperties.md#label), [collect](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingProperties.md#collect), [classify](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingProperties.md#classify) or [tabulate](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingProperties.md#tabulate).

Here is an example on how to use [classify](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingProperties.md#classify):
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

To fail a test with insufficient coverage use [checkCoverage](https://github.com/1Jajen1/propCheck#checkcoverage) with functions like [cover](https://github.com/1Jajen1/propCheck#cover) or [coverTable](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingProperties.md#covertable).
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

> There are plans of adding better support for test runners so that you can drop the propCheck {} wrapper.

## Use of arrow in and with propCheck

First of all: If you are using [arrow-kt](https://arrow-kt.io/): Great! There are plenty of instances already defined for arrows data-types.

If not then don't worry. Most of the api can be used without ever touching upon arrows datatypes and there are overloads specifically to avoid those cases if they do come up.
> That is excluding types like Option or TupleN, which should be fine.

## Kotlintest generators vs propCheck

Another library that offers property-based testing is kotlintest, however it has several drawbacks:
- Shrinking is much worse.
    - Shrinking instances are rather primitive (especially lists). propCheck uses the same methods quickCheck uses, and quickcheck is built upon years and years of experience and research.
        - This causes kotlintests shrunk results to be worse for most non-trivial shrinking operations (and even in the trivial cases)
    - Generators are tied to shrinkers but most useful operations on generators silently drop the shrinker. This is quite problematic for several reasons, it makes using shrinking much harder as you have to avoid certain interface methods to keep it. Also it makes reasoning about code harder because it's silent! `Gen.map(id)` should not change the datatype, in kotlintest it will, because it forgets the shrinker
    - Operates over over lists rather than lazy sequences, this will become a problem with shrinking larger examples
- It is just less powerful. propCheck offers generating functions (properly), coverage checks on your generated data, statemachine testing for complex stateful systems (with support for catching race-conditions), and more smaller things.
- Not very flexible. propCheck offers many more combinators for both properties, generators and shrinkers
- While kotlintest does allow reproducable tests (by seeding a specific generator), propCheck takes this quite a step further, not associating a generator with a random seed, but having the whole property test being deterministic after seeding it. Retaining good random values is achieved by using a splittable random generator.
- Kotlintest also has no notion of growing it's input over time. propCheck starts random generation with smaller values and increases to fail faster. This also allows precise control over the size of generated structures (needed for anything recursive, like lists and trees etc)

However propCheck also has some drawbacks:
- Exception handling has to go through IO/suspend or needs to be handled manually. Not a huge problem, but for exception heavy code can be a burden
- Kotlintests api is easier to use in trivial cases (but that is also what limits it)

Outside of property based testing kotlintest still makes a fine testing library, and I recommend using it to execute the tests and for anything that is not a property test!

## Feedback
`propCheck` is still in its early days so if you notice bugs or think something can be improved please create issues or shoot me a pull request. All feedback is highly appreciated.

## Credits
`propCheck` is a port of the awesome library [quickcheck](https://github.com/nick8325/quickcheck). If you ever come around to use haskell make sure to give it a go!
Writing `propCheck` was also made much easier by using [arrow-kt](https://arrow-kt.io/) to be able to write code in a similar style to haskell and thus close to the original.
As with `quickCheck` make sure to check out `arrow` for a more functional programming style in kotlin.
