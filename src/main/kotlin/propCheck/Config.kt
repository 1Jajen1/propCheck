package propCheck

import arrow.fx.IO
import arrow.fx.extensions.io.applicative.applicative
import arrow.fx.fix

data class Config(
    val useColor: UseColor,
    val verbose: Verbose
)

sealed class Verbose {
    object Quiet: Verbose()
    object Normal: Verbose()
}

sealed class UseColor {
    object EnableColor: UseColor()
    object DisableColor: UseColor()
}

inline class TaskId(val id: Int)

// look for env options and check terminal capabilities, env options have precedence
// TODO
fun detectColor(): IO<UseColor> = IO.just(UseColor.EnableColor)

fun detectVerbosity(): IO<Verbose> = IO.just(Verbose.Quiet)

fun detectConfig(): IO<Config> = IO.applicative().map(
    detectColor(),
    detectVerbosity()
) { (useColor, verbosity) -> Config(useColor, verbosity) }.fix()


