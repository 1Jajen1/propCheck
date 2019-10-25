# Creating properties to test

## Table of contents:
* [Overview](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingProperties.md#overview)
* [Creating properties](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingProperties.md#creating-properties)
* [Combinators](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingProperties.md#combinators)
    * [Inspecting test data](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingProperties.md#interacting-with-test-cases)
    * [Interacting with test cases](https://github.com/1Jajen1/propCheck/blob/master/docs/CreatingProperties.md#interacting-with-test-cases)

### Overview
As seen in the [introduction](https://github.com/1Jajen1/propCheck/blob/master/docs/Introdution.md) a property usually starts with a `forAll` and contains some, usually boolean, condition.

To express some more complex properties we'll take a look at all the combinators that propCheck offers.

### Creating properties
At it's core a `Property` is just a wrapped generator of test results, as such it can be created from every type that has an instance of `Testable<A>`, which at the time of writing this are: `Boolean`, `TestResult` and `Property`.

Most combinators include overloads for `Boolean` so that instead of writing `combinator(Boolean.testable().run { true.property() })` one can just write `combinator(true)`.

### Combinators

#### forAll
Run a property test given some parameters from `forAll`. It has a few overloads:

`forAll(arb: Arbitrary<A>, ..., f: (A) -> Property)`  delegates to `forAllShrink` with the generator and shrinker provided by `Arbitrary<B>`.
It can also lookup a default instance for Arbitrary (only for basic datatypes), so that all you have to specify is the type.

```kotlin
propCheck {
    forAll { i: Int ->
        i < 0
    }
}
// prints =>
*** Failed! (after 1 test):
Falsifiable
0
```

`forAll(gen: Gen<A>, ..., f(A) -> Property)` forAll with an explicit generator, does no shrinking.

```kotlin
propCheck {
    forAll(arbitrarySizedInt(), Boolean.testable()) { i ->
        i < 0
    }
}
// prints =>
*** Failed! (after 1 test):
Falsifiable
0
```

Usually the ide is good enough at choosing the right method, so it's best to just start typing and not worrying about it.

`forAll` can also safely be nested to avoid having to cram everything into Tuple's.

#### forAllShrink

`forAll` but with an added parameter `(B) -> Sequence<B>` that shrinks failing cases.

```kotlin
propCheck {
    forAllShrink(arbitrarySizedInt(), ::shrinkInt, Boolean.testable()) { i ->
        i < 10
    }
}
// prints =>
*** Failed! (after 21 tests and 1 shrink):
Falsifiable
10
```

#### forAllBlind

`forAll` but does not output the parameters on failure.

```kotlin
propCheck {
    forAllBlind { i: Int ->
        i < 10
    }
}
// prints =>
*** Failed! (after 16 tests and 1 shrink):
Falsifiable
```

#### forAllShrinkShow

`forAll` but with an explicit show function `(B) -> String` to show the parameters. (Usually defaults to `Show.any()` which uses `toString()` in the other `forAll` methods)

```kotlin
propCheck {
    forAllShrinkShow(arbitrarySizedInt(), ::shrinkInt, { "i: $it" }, Boolean.testable()) { i ->
        i < 10
    }
}
// prints =>
*** Failed! (after 17 tests and 3 shrinks):
Falsifiable
i: 10
```

#### forAllShrinkBlind

`forAllShrink` but without printing the parameters.

---

#### eqv / neqv

Compare two values for equality, adding a nice printout on failure.
Can be supplied with custom `Eq<A>` but has a default to use == from kotlin.

```kotlin
propCheck {
    forAll { (i, j): Pair<Int, Int> ->
        (i - j).eqv(j - i)
    }
}
// prints =>
*** Failed! (after 3 tests and 2 shrinks):
Falsifiable
(0,1)
Expected: -1 to be equal to:
        : 1
```

---

#### and

Combine two `Properties` failing if one or both fail. Also includes overloads with `Eval<Property>` and `() -> Property` to short circuit evaluation if failure is guaranteed.

#### or

Combine two `Properties` failing only if both fail. Also includes lazy overloads like `and` to short circuit if success is guaranteed.

---

#### counterexample

Add a custom error text that is displayed on failure. The text is lazy, so even if you have a data-type that is expensive to print, you should be fine since this is only called on failure. (Including shrink failures)

```kotlin
propCheck {
    forAll { (i, j): Pair<Int, Int> ->
        counterexample(
            { "Custom descriptive text" },
            (i + j == j + i)
        )
    }
}
```

---

#### expectFailure

Invert the property. There are some exceptions to using this together with `and`/`or`, so just make sure to keep this top-level.

---

#### once

Stop testing after this test.

#### again

Continue testing after this test.

`once` and `again` are mostly implementation details of `forAll`, the interaction of these used together can be a bit weird.

---

#### verbose

More output when running tests. Usually prints intermediate results for each test run.

#### verboseShrinking

Same as `verbose` but even prints while testing shrunk cases.

---

### Inspecting test-data

Testdata generated with `forAll` is just a sample of the set of possible values. This directly relates the quality of the test to the quality of the generator used in `forAll`.

propCheck contains some methods of statistical analysis to better undestand how inputs are distributed and to fail if that distribution is not satisfying.

#### label

Attach a specific label to a property.

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
> In this case `classify` will be easier to use.

#### classify

Attach a label to a property if it passes the supplied condition.

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

#### collect

Like label, but automatically creates the label from the supplied data. Useful for debugging when you want to see all values generated.

```kotlin
propCheck {
    forAll { (i): NonNegative<Int> ->
        collect(
            i.rem(2),
            i >= 0
            )
    }
}
// prints =>
+++ OK, passed 100 tests:
56,00% 1
44,00% 0
```

#### tabulate
 
Label test data with the ability to put the labels into different tables.

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

#### cover

Add a coverage check to the testcase, but does not fail the test on its own (only inside `checkCoverage` TODO LINK). Simply adds output if the coverage is insufficient.

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

#### coverTable

Add a coverage check to specific a table of labels. For that table it works the same as `cover`.

```kotlin
propCheck {
    forAll { intArr: IntArray ->
        tabulate(
            "Data",
            listOf(if (intArr.isNotEmpty()) "not empty" else "empty"),
            coverTable(
                "Data",
                listOf("not empty" toT 90.0),
                true
            )
        )
    }
}
// prints =>
+++ OK, passed 200 tests.
Data (200 in total)
99,00% not empty
1,00% empty
```
 > The order should not be important here.
 
 #### checkCoverage
 
 Adds failure if any coverage check inside the property fails. Can be given parameters to tweak behaviour slightly.
 
 #### discardIf
 
 Discard a test-result. This test is neither a failure nor a success, but if too many cases get discarded propCheck will fail.
 This can also seriously mess up the input data, so check if the above methods to make sure the data is fine.

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
 
---

### Interacting with test cases

Sometimes it can be useful to execute some code after test cases fail.
The smallest primitve for that is `callback` but higher-level wrapper methods make life easier.

#### callback

Append a callback that is executed after every failed test case. For example counterexample is implemented using callbacks.

#### whenFail/whenFailIO

Execute code (with or without side-effects) after the final test-failure.

#### whenFailEvery/whenFailEveryIO

Execute code (with or without side-effects) after every test-failure.