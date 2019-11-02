package propCheck

import io.kotlintest.AbstractSpec
import io.kotlintest.TestType
import io.kotlintest.specs.IntelliMarker
import propCheck.property.Property

abstract class AbstractPropertySpec(f: AbstractPropertySpec.() -> Unit = {}) : AbstractSpec() {
    init { f() }

    operator fun String.invoke(args: Args = Args(), f: () -> Property): Unit =
            addTestCase(
                this,
                {
                    propCheck(args, f)
                },
                defaultTestCaseConfig,
                TestType.Test
            )
}

abstract class PropertySpec(f: AbstractPropertySpec.() -> Unit = {}): AbstractPropertySpec(f), IntelliMarker