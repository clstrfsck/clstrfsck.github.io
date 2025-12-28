import java.io.File

val result1 = File("y25d12.txt").readText().split("\n\n").last().lines()
    .map { it.split(Regex("\\D+")).map(String::toInt) }
    .count { l -> l.take(2).map { it / 3 }.reduce(Int::times) >= l.drop(2).sum() }

println("Part1: $result1")