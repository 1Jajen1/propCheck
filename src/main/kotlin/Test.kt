import arrow.core.Tuple3
import arrow.core.toT
import arrow.data.ListK
import arrow.data.k
import propCheck.*
import propCheck.assertions.forAll
import propCheck.assertions.tabulate
import propCheck.gen.applicative.applicative
import propCheck.gen.monad.binding
import propCheck.instances.arbitrary
import propCheck.instances.listk.arbitrary.arbitrary
import propCheck.instances.tuple3.arbitrary.arbitrary

fun main() {
    propCheck {
        forAll { i: Int ->
            tabulate(
                "Data",
                listOf("Some label", i.toString()),
                i + i == 2 * i
            )
        }
    }
}

sealed class BinaryTree {
    data class Leaf(val i: Int) : BinaryTree()
    data class Branch(val left: BinaryTree, val right: BinaryTree) : BinaryTree()
}

fun binaryTreeGen(): Gen<BinaryTree> = Gen.sized { size ->
    if (size == 1) arbitrarySizedInt().map { BinaryTree.Leaf(it) as BinaryTree }
    else Gen.frequency(
        1 toT arbitrarySizedInt().map { BinaryTree.Leaf(it) as BinaryTree },
        3 toT binding {
            BinaryTree.Branch(
                binaryTreeGen().resize(size - 1).bind(),
                binaryTreeGen().resize(size - 1).bind()
            ) as BinaryTree
        }.fix()
    )
}

val listGen = arbitrarySizedInt().listOf().suchThat { it.isNotEmpty() }.map { it.sorted() }

data class User(val name: String, val age: Int, val friends: List<String>)

val userArb = object : Arbitrary<User> {
    override fun arbitrary(): Gen<User> = Gen.applicative().map(
        arbitraryASCIIString(),
        arbitrarySizedInt(),
        ListK.arbitrary(String.arbitrary()).arbitrary()
    ) { (name, age, friends) ->
        User(name, age, friends)
    }.fix()

    override fun shrink(fail: User): Sequence<User> =
        shrinkMap({ user ->
            Tuple3(user.name, user.age, user.friends.k())
        }, { (name, age, friends) ->
            User(name, age, friends)
        }, Tuple3.arbitrary(String.arbitrary(), Int.arbitrary(), ListK.arbitrary(String.arbitrary()))).invoke(fail)
}