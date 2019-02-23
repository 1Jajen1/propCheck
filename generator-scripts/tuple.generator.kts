import java.io.File

val notice = """/*
  AUTOGENERATED FILE. DON'T EDIT DIRECTLY
  This file was generated by tuple.generator.kts
*/
"""

val package_ = "abcd.instances"

val maxTuple = 21

val availableLetters = ('a'..'z').toList().map { it.toString() }

val fileContent = StringBuilder()

val imports = "import abcd.*\n" +
        "import arrow.core.*\n" +
        "import abcd.gen.monad.monad\n" +
        (2..maxTuple).map { "import abcd.instances.tuple${it}.arbitrary.arbitrary" }.joinToString("\n") +
        "\nimport arrow.extension"

fileContent.append(notice)
fileContent.append("package $package_")
fileContent.append("\n\n")
fileContent.append(imports)
fileContent.append("\n\n")

fun unwrapTuple(i: Int, j: Int, letters: List<String>) : String {
    val build = StringBuilder()
    if (0 == i) build.append("it")
    else build.append("fail.a")
    for (k in 1..(j - 1)) {
        if (k == i) build.append(", it")
        else build.append(", fail.${letters[k]}")
    }
    return build.toString()
}


for (i in 2..maxTuple) {

    val letters = availableLetters.take(i)

    val diamond1 = "<${letters.joinToString { it.toUpperCase() }}>"

    fileContent.append("@extension").append("\n")
    fileContent.append("interface Tuple${i}Arbitrary${diamond1} : Arbitrary<Tuple${i}${diamond1}> {").append("\n")

    letters.forEach { l ->
        fileContent.append("    fun A${l.toUpperCase()}(): Arbitrary<${l.toUpperCase()}>").append("\n")
    }

    fileContent.append("    override fun arbitrary(): Gen<Tuple${i}${diamond1}> = Gen.monad().fx {").append("\n")
    fileContent.append("        Tuple${i}(AA().arbitrary().bind()")
    letters.drop(1).forEach { l ->
        fileContent.append(", A${l.toUpperCase()}().arbitrary().bind()")
    }
    fileContent.append(")").append("\n")
    fileContent.append("    }.fix()").append("\n")

    fileContent.append("    override fun shrink(fail: Tuple${i}${diamond1}): Sequence<Tuple${i}${diamond1}> =").append("\n")
    fileContent.append("        AA().shrink(fail.a).map { Tuple${i}(${unwrapTuple(0, i, letters)}) }")
    letters.drop(1).forEachIndexed() { j, l ->
        fileContent.append(" +\n")
        fileContent.append("            A${l.toUpperCase()}().shrink(fail.${l}).map { Tuple${i}(${unwrapTuple((j + 1), i, letters)}) }")
    }
    fileContent.append("\n")
    fileContent.append("}").append("\n\n")

    fileContent.append("inline fun <${letters.joinToString { "reified " + it.toUpperCase() }}, F1> fromTup(").append("\n")
    fileContent.append("    noinline g: (F1) -> Tuple${i}${diamond1},").append("\n")
    fileContent.append("    noinline f: (Tuple${i}${diamond1}) -> F1,").append("\n")
    fileContent.append("    arb: Arbitrary<Tuple${i}${diamond1}> = Tuple${i}.arbitrary(defArbitrary()")
    (2..i).forEach {
        fileContent.append(", defArbitrary()")
    }
    fileContent.append(")")
    if (i > 2) {
        (3..i).forEach {
            fileContent.append(",\n").append("    dummy$it: Unit = Unit")
        }
    } else fileContent.append("\n")

    fileContent.append("): Arbitrary<F1> = object : Arbitrary<F1> {").append("\n")
    fileContent.append("    override fun arbitrary(): Gen<F1> = arb.arbitrary().map(f)\n")
    fileContent.append("    override fun shrink(fail: F1): Sequence<F1> = shrinkMap(g, f, arb).invoke(fail)\n")
    fileContent.append("}\n")

    fileContent.append("\n")
}

val fileLocation = "../src/main/kotlin/io.jannis.propTest/instances/Tuples.kt"

File(fileLocation).writeText(fileContent.toString())