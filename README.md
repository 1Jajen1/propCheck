# propCheck - Property based testing in kotlin

[![CircleCI](https://circleci.com/gh/1Jajen1/propCheck/tree/master.svg?style=svg)](https://circleci.com/gh/1Jajen1/propCheck/tree/master)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Download](https://api.bintray.com/packages/jannis/propCheck-kt/propCheck-kt/images/download.svg) ](https://bintray.com/jannis/propCheck-kt/propCheck-kt/_latestVersion)

> A small library built upon kotlintest that aims to adopt haskell [Quickcheck's](https://github.com/nick8325/quickcheck) functionality.
It started out as a straight port of quickcheck and was adapted slightly to work better with kotlin

## Table of contents

* [Usage](https://github.com/1Jajen1/propCheck#usage)
* [Shrinking](https://github.com/1Jajen1/propCheck#shrinking)
* [Testing custom classes](https://github.com/1Jajen1/propCheck#testing-custom-classes)
* [The `Gen<A>` datatype](https://github.com/1Jajen1/propCheck#the-gena-datatype)
* [A note regarding test data](https://github.com/1Jajen1/propCheck#a-note-regarding-test-data)
* [State-machine-testing and testing for race-conditons](https://github.com/1Jajen1/propCheck#state-machine-testing-and-testing-for-race-conditions)
* [Running with a test runner](https://github.com/1Jajen1/propCheck#running-these-tests-with-a-test-runner)
* [Use of arrow in and with propCheck](https://github.com/1Jajen1/propCheck#use-of-arrow-in-and-with-propcheck)
* [Kotlintest generators vs propCheck](https://github.com/1Jajen1/propCheck#kotlintest-generators-vs-propcheck)
* [Feedback](https://github.com/1Jajen1/propCheck#feedback)
* [Credits](https://github.com/1Jajen1/propCheck#credits)
   
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

In the following example we wirite a simple generator for a user data class:
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
Here we combine three generators (String, Int, List<String>) and map their result to a user.
> This is also using the invoke constructor from `Arbitrary`.

Let's add a way to shrink this user:
```kotlin
val userArb = object : Arbitrary<User> {
    override fun arbitrary(): Gen<User> = ...
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
propCheck {
    forAll(userArb) { user ->
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
* use [fromTup](https://github.com/1Jajen1/propCheck#fromtupto-a---tuplen-from-tuplen---a-arbitrarytuple2n) which given a function from and to a TupleN returns an Arbitrary instance with shrinking
* use [arrow-generic](https://arrow-kt.io/docs/generic/product/) to auto generate from and to tuple functions
* use [defArbitrary](https://github.com/1Jajen1/propCheck/blob/master/README.md#defarbitrarya-arbitrarya) which can for most `A`'s infer a Arbitrary instance. A full list can be seen [here](https://github.com/1Jajen1/propCheck/blob/master/README.md#types-with-default-implementations).

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

To inspect results use either [label](https://github.com/1Jajen1/propCheck#label), [collect](https://github.com/1Jajen1/propCheck#collect), [classify](https://github.com/1Jajen1/propCheck#classify) or [tabulate](https://github.com/1Jajen1/propCheck#tabulate).

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

## State-machine-testing and testing for race-conditions

State-machine-testing is the concept of verifying actions against a stateful system by running them against a simpler model.
A very simple example is a ticket machine:
```kotlin
data class TicketMachine(var lastTicketNr: Int = 0) {
    fun takeTicket(): Int = ++lastTicketNr
    fun reset() {
        lastTicketNr = 0
    }
}

sealed class TicketAction {
    object Take: TicketAction()
    object Reset: TicketAction()
    
    override fun toString(): String = when (this) {
        is Take -> "Take"
        is Reset -> "Reset"
    }
}

val actionGen = Gen.elements(TicketAction.Take, TicketAction.Reset)

val stateMachine = StateMachine(
    initialState = 0,
    invariant = { state -> state >= 0 },
    preCondition = { state: Int, action: TicketAction -> true },
    cmdGen = { state -> actionGen.map { it.some() } },
    sut = { IO { TicketMachine() } },
    transition = { state, action ->
        when (action) {
            is TicketAction.Take -> state + 1
            is TicketAction.Reset -> 0
        }
    },
    executeAction = { action, sut ->
        IO {
            when (action) {
                is TicketAction.Take -> sut.takeTicket()
                is TicketAction.Reset -> { sut.reset(); 0 }
            }
        }
    }
)

fun main() {
    propCheck {
        execSeq(stateMachine) { prevState, action, result ->
            when (action) {
                is TicketAction.Take -> prevState + 1 == result
                is TicketAction.Reset -> true
            }
        }
    }
}
// prints =>
+++ OK, passed 100 tests.
```

Now this ticket-machine is tested against sequential execution of all possible actions, but that is not really the point of a ticket machine. It is also supposed to work in parallel! However that may lead to some interesting situations, for example when 2 people taking tickets get the same number, that would be a race-condition, and since the above example has no synchronization it will eventually happen. But we can test for that:
```kotlin
// This is using the exact same code as above
fun main() {
    propCheck {
        execPar(stateMachine) { prevState, action, result ->
            when (action) {
                is TicketAction.Take -> prevState + 1 == result
                is TicketAction.Reset -> true
            }
        }
    }
}
// prints something like this =>
*** Failed! (after 14 tests and 1 shrink):
Falsifiable
No possible interleaving found for: 
Path 1: Take -> 1, Reset -> 0, Take -> 2
Path 2: Take -> 1, Take -> 1, Reset -> 0
```

As you can see take from two threads both received ticket number 1, which is not acceptable and propCheck was able to find that and shrink to a smaller example!

### How does this work?!

Behind the scenes propCheck runs randomly generated lists of commands in parallel against the system and records the results. Then after execution it tries to find a sequential path in which the model proves right for the results. In the above example there is just no order/interleaving to execute the commands that result in a correct state.

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
Kotlintest already includes some means of property based testing. However their data-types and reflection-lookups are severly limited. The reason I ported quickcheck over is because it is built upon lawful instances for its datatypes and features a much richer set of methods. This makes testing with propCheck much easier and much more powerful.

## Feedback
`propCheck` is still in its early days so if you notice bugs or think something can be improved please create issues or shoot me a pull request. All feedback is highly appreciated.

## Credits
`propCheck` is a port of the awesome library [quickcheck](https://github.com/nick8325/quickcheck). If you ever come around to use haskell make sure to give it a go!
Writing `propCheck` was also made much easier by using [arrow-kt](https://arrow-kt.io/) to be able to write code in a similar style to haskell and thus close to the original.
As with `quickCheck` make sure to check out `arrow` for a more functional programming style in kotlin.
