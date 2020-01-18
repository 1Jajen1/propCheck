package propCheck.pretty

import arrow.fx.IO

const val ESC = "\u001B"

/*

 Need a way to section the console and clear individual sections

 */

fun clearConsole(): IO<Unit> = IO {
    print("$ESC[H$ESC[2J")
}