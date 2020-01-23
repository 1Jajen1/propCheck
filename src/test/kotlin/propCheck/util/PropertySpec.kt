package propCheck.util

import arrow.core.Tuple2
import arrow.core.some
import io.kotlintest.AbstractSpec
import io.kotlintest.TestType
import io.kotlintest.specs.IntelliMarker
import propCheck.*
import propCheck.property.*

abstract class AbstractPropertySpec(f: AbstractPropertySpec.() -> Unit = {}) : AbstractSpec() {
    init { f() }

    operator fun String.invoke(props: List<Tuple2<String, Property>>): Unit =
        addTestCase(
            this,
            {
                checkGroup(this@invoke, props)
                    .unsafeRunSync()
                    .let {
                        if (it.not()) throw AssertionError("Some tests failed!")
                    }
            },
            defaultTestCaseConfig,
            TestType.Test
        )

    operator fun String.invoke(
        propertyConfig: PropertyConfig = PropertyConfig(),
        c: suspend PropertyTestSyntax.() -> Unit
    ): Unit =
        addTestCase(
            this,
            {
                checkReport(PropertyName(this@invoke).some(), property(propertyConfig, c))
                    .unsafeRunSync()
                    .toException()
            },
            defaultTestCaseConfig,
            TestType.Test
        )

    operator fun String.invoke(
        args: Config,
        propertyConfig: PropertyConfig = PropertyConfig(),
        c: suspend PropertyTestSyntax.() -> Unit
    ): Unit =
        addTestCase(
            this,
            {
                checkReport(
                    args,
                    PropertyName(this@invoke).some(),
                    property(propertyConfig, c)
                )
                    .unsafeRunSync()
                    .toException()
            },
            defaultTestCaseConfig,
            TestType.Test
        )

    operator fun String.invoke(f: Property): Unit =
        addTestCase(
            this,
            {
                checkReport(PropertyName(this@invoke).some(), f)
                    .unsafeRunSync()
                    .toException()
            },
            defaultTestCaseConfig,
            TestType.Test
        )

    operator fun String.invoke(args: Config, f: Property): Unit =
        addTestCase(
            this,
            {
                checkReport(args, PropertyName(this@invoke).some(), f)
                    .unsafeRunSync()
                    .toException()
            },
            defaultTestCaseConfig,
            TestType.Test
        )
}

fun Report<Result>.toException(): Unit = when (status) {
    is Result.Success -> Unit
    is Result.GivenUp -> throw AssertionError("GaveUp!")
    is Result.Failure -> throw AssertionError("Failed!")
}

abstract class PropertySpec(f: AbstractPropertySpec.() -> Unit = {}): AbstractPropertySpec(f), IntelliMarker
