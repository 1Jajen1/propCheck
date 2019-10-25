# Statemachine testing

## Table of contents
* [Intro]()
* [Finding race conditions]()
* [More!]()

### Intro

Property based testing works best on small, pure functions, but it lacks when it comes to testing effectful functions and even more so when multiple-threads are thrown into the mix.

One way to overcome those limitations and still enjoy the benefits of property based testing is to model the system to test as a state-machine and provide generators for actions and rules for your system.

This is best shown as a simple example using a ticket machine: (This is quite a bit of boilerplate sadly, but worth the effort!)
```kotlin
// System to test
data class TicketMachine(var lastTicketNr: Int = 0) {
    fun takeTicket(): Int = ++lastTicketNr
    fun reset() {
        lastTicketNr = 0
    }
}

// state transitions
sealed class TicketAction {
    object Take: TicketAction()
    object Reset: TicketAction()
    
    override fun toString(): String = when (this) {
        is Take -> "Take"
        is Reset -> "Reset"
    }
}

// generator for transitions
val actionGen = Gen.elements(TicketAction.Take, TicketAction.Reset)

val stateMachine = StateMachine(
    // initial reference model state
    initialState = 0,
    // a global invariant that there should never be fewer than 0 tickets
    invariant = { state -> state >= 0 },
    // a preCondition: constant in this case, anything goes
    preCondition = { state: Int, action: TicketAction -> true },
    // command generator, use None to stop generating if a terminal action is reached
    cmdGen = { state -> actionGen.map { it.some() } },
    // Initializing the system to test (sut = system under test)
    sut = { IO { TicketMachine() } },
    // a state machine transition function
    transition = { state, action ->
        when (action) {
            is TicketAction.Take -> state + 1
            is TicketAction.Reset -> 0
        }
    },
    // performing actions against the sut
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
        // execute by providing it a statemachine and a post-condition for the state-machine
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

### Finding race conditions

The above code works perfectly fine in single-threaded contexts, but due to a lack of locking will fail in concurrent systems, but is there a way to test and prove that?

Well yes, taking the above example one step further we can instead of only generating a single list of actions, generate multiple lists and execute them in parallel. Then we'd only have to prove that from the results there is no path we can take that satisfies the precondition.

Here is that in action:
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

As we can see, two threads both took a ticket at the same time and thus got number 1 as a result. But there is no linear way to order the actions such that that would make sense.

### More!

I highly recommend watching [this](https://vimeo.com/68383317) talk by John Hughes about this very topic (only in Erlang) 
