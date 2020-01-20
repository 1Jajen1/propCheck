package propCheck.pretty

import arrow.core.*
import arrow.core.extensions.list.foldable.fold
import arrow.core.extensions.list.traverse.traverse
import arrow.core.extensions.listk.foldable.foldMap
import arrow.core.extensions.listk.monoid.monoid
import arrow.core.extensions.monoid
import arrow.core.extensions.sequence.zip.zipWith
import arrow.fx.*
import arrow.fx.extensions.fx
import arrow.fx.extensions.io.applicative.applicative
import arrow.fx.extensions.io.concurrent.concurrent
import arrow.fx.extensions.io.dispatchers.dispatchers
import arrow.fx.extensions.io.monad.monad
import arrow.fx.typeclasses.ExitCase
import arrow.fx.typeclasses.Fiber
import arrow.fx.typeclasses.seconds
import java.io.PrintStream

/*

TODO:
    - Bracket printstream
    - check buffer size before writing
    - autoflush buffer? How do I handle newlines?
     - Always append newlines, but remember somehow to remove them later? Keep the line that we appended a newline to
        and readd it later?
 */

suspend fun main() {
    IO.fx {
        dispatchers().io().raceN(timer().sleep(10.seconds).followedBy(effect { println("Hello") }), effect { println("Hello world") })
            .bind()
    }.unsafeRunSync()

    var i = 0
    var j = 0
    displayConcurrently { createRegion, setRegion, closeRegion ->
        IO.fx {
            val r2 = createRegion().bind()
            val r = createRegion().bind()
            IO.fx {
                setRegion(r, "I: ${i++}").bind()
                setRegion(r2, "J: ${j++}").bind()
            }.repeat(IO.concurrent(), Schedule.withMonad(IO.monad()) { recurs<Unit>(2) }).unit().bind()

            closeRegion(r, "Result: " + r.body.get().bind()).bind()

            IO.fx {
                setRegion(r2, "J: ${j++}").bind()
            }.repeat(IO.concurrent(), Schedule.withMonad(IO.monad()) { recurs<Unit>(2) }).unit().bind()

            closeRegion(r2, "Result: " + r2.body.get().bind()).bind()

            effect { println("Done") }.bind()
        }
    }.suspended()
}

data class Region(
    val body: Ref<ForIO, String>
)

sealed class RegionChange {
    data class BufferChange(val buff: ByteArray, val limit: Int) : RegionChange()
    object RegionContent : RegionChange()
    object ShutDown : RegionChange()
}

// invariant, only called once at a time! TODO add some global var or so to check it first
fun displayConcurrently(
    f: (
        createRegion: () -> IO<Region>,
        setRegion: (Region, String) -> IO<Unit>,
        closeRegion: (Region, String) -> IO<Unit>
    ) -> IO<Unit>
): IO<Unit> = setup().bracket({ (_, _, _, r, f, w, n, q) ->
    cleanup(r, f, w, n, q)
}, { (create, set, close) -> f(create, set, close) })

fun setup() = IO.fx {
    val regions = arrow.fx.extensions.io.monadDefer.Ref(emptyList<Region>()).bind()
    val endPromise = Promise.invoke<ForIO, Unit>(IO.concurrent()).bind()
    // detect terminal capabilities here

    // native sysout
    val nativeOut = System.out

    val queue = Queue.unbounded<ForIO, RegionChange>(IO.concurrent()).bind()
    val redraw = MVar.empty<ForIO, Unit>(IO.concurrent()).bind()

    fun update(currentOut: List<String>): IO<Unit> = IO.fx {
        val change = IO.fx {
            val sz = queue.size().bind()
            // make sure queue is empty before closing
            // if (sz != 0) queue.take().bind()
            /* else */ IO.dispatchers().default().raceN(effect { "??" }, effect { "!!" }).bind().fold(
                { RegionChange.RegionContent }, { RegionChange.ShutDown }
            )
        }.bind()

        when (change) {
            is RegionChange.BufferChange -> {
                val clear = cursorUp(currentOut.size) + setCursorColumn(0) + clearFromCursorToScreenEndCode
                effect { nativeOut.print(clear) }.bind()
                effect { nativeOut.write(change.buff, 0, change.limit) }.bind()
                effect { if (currentOut.isNotEmpty()) nativeOut.println(currentOut.joinToString("\n")) }.bind()
                effect { nativeOut.flush() }.bind()

                update(currentOut).bind()
            }
            is RegionChange.RegionContent -> {
                // get the new output, compare it with the old and print an edit string
                val regions = regions.get().bind()
                val newOut = regions.traverse(IO.applicative()) { it.body.get() }.bind()
                    .foldMap(String.monoid()) { if (it.isEmpty()) "" else "$it\n" }.dropLast(1)

                if (currentOut.isEmpty() && newOut.isEmpty())
                    update(emptyList()).bind()
                else if (newOut.isEmpty()) updateDisplay(nativeOut, currentOut, emptyList())
                    .followedBy(update(emptyList())).bind()
                else newOut.split("\n").let {
                    updateDisplay(nativeOut, currentOut, it)
                        .followedBy(update(it)).bind()
                }
            }
            is RegionChange.ShutDown -> Unit
        }
    }

    val fiber = update(emptyList()).fork(dispatchers().io()).bind()

    val newOutStream: PrintStream = object : PrintStream(nativeOut, true) {
        var buffer: ByteArray = ByteArray(1024)
        var offset = 0

        override fun write(p0: ByteArray, p1: Int, p2: Int) {
            // TODO ensure size?!
            p0.copyInto(buffer, offset, p1, p2)
            offset += p2 - p1
        }

        override fun flush() {
            nativeOut.println("Flush start")
            val change = RegionChange.BufferChange(buffer.copyOf(), offset)
            nativeOut.println("Flushing: ${buffer.slice(0..offset).joinToString("") { it.toChar().toString() }}")
            queue.offer(change).fix().unsafeRunSync()
            nativeOut.println("Flush end")
            // Ugly stuff ^^
            buffer = ByteArray(1024)
            offset = 0
        }
    }

    // replace stdout with a custom implementation where I have access to the buffer and control when to flush
    effect { System.setOut(newOutStream) }.bind()

    fun createRegion(): IO<Region> = IO.fx {
        val regionRef = Ref.invoke(IO.concurrent(), "").bind()
        val region = Region(regionRef)

        regions.modify { it + region toT Unit }.bind()

        region
    }

    fun setRegion(region: Region, str: String): IO<Unit> = IO.fx {
        region.body.set(str).bind()

        redraw.tryPut(Unit).unit().bind()
    }

    fun closeRegion(region: Region, last: String): IO<Unit> = IO.fx {
        regions.modify { it.filter { it != region } toT Unit }.bind()

        effect { newOutStream.println(last) }.bind()
        effect { newOutStream.flush() }.bind()
    }

    Tuple8(::createRegion, ::setRegion, ::closeRegion, endPromise, fiber, newOutStream, nativeOut, queue)
}

fun cleanup(promise: Promise<ForIO, Unit>, fiber: Fiber<ForIO, Unit>, wrapped: PrintStream, nativeOut: PrintStream, queue: Queue<ForIO, RegionChange>): IO<Unit> = IO.fx {
    // TODO bracket wrapping and unwrapping of stdout because this should not actually run in here!
    effect { wrapped.flush() }.bind()
    effect { System.setOut(nativeOut) }.bind()
    println("System out reset")
    promise.complete(Unit).bind()
    fiber.join().unit().bind()
}

// TODO maybe add a smarter update algorithm that skips unchanged lines and skips unchanged columns in a line
fun updateDisplay(handle: PrintStream, old: List<String>, new: List<String>): IO<Unit> = IO.effect {
    val outStr = cursorUp(old.size) +
            setCursorColumn(0) +
            clearFromCursorToScreenEndCode +
            new.joinToString("\n")
    handle.println(outStr)
    handle.flush()
}

fun cursorUp(n: Int): String = listOf(n).csi("A")
fun cursorDown(n: Int): String = listOf(n).csi("B")
fun cursorForward(n: Int): String = listOf(n).csi("C")
fun cursorBack(n: Int): String = listOf(n).csi("D")
fun cursorDownLine(n: Int): String = listOf(n).csi("E")
fun cursorUpLine(n: Int): String = listOf(n).csi("F")
fun setCursorColumn(n: Int): String = listOf(n).csi("G")

val clearFromCursorToScreenEndCode = listOf(0).csi("J")
val clearFromCursorToLineEndCode = listOf(0).csi("K")
val clearLineCode = listOf(2).csi("K")

private const val ESC = "\u001B"

private fun List<Int>.csi(code: String): String = "$ESC[" + map { it.toString() }
    .intersperse(";").joinToString("") + code

private fun <A> List<A>.intersperse(a: A): List<A> = when {
    isEmpty() -> this
    else -> (first() toT drop(1)).let { (x, xs) ->
        listOf(x) + (
                arrow.core.extensions.sequence.repeat.repeat(a).zipWith(xs.asSequence()) { l, r -> listOf(l, r).k() }
                ).toList().fold(ListK.monoid())
    }
}
