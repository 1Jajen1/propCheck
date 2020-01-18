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

// model:
// lifecycle hopes just deliver state of one specific test, nothing more, they don't give a shit on how
// the thing is handled

// check outputs straight to console
// checkNamed outputs straight to console but with the name
// recheck outputs straight to console
// checkGroup outputs the whole state to console, all concurrent runs etc
// checkSequential and checkPar just call checkGroup

// if run using a gradle plugin we use checkNamed + throwOnFailure

// check = setupConfig >>= genSeed >>= runProperty (returns true if successfull)
// lifecycle only ever prints on completion, never when running

// checkNamed = check, but with a lifecycle hook that appends the name to the report

// recheck = check with custom seed and size and no return type

// checkGroup = TODO Do this after the others


