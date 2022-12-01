```scala
@main def main(arg: String): Unit =
  import scala.io.Source
  val data = Source
    .fromFile(arg)
    .getLines
    .toList
    .mkString(" ")
    .split("  ")
    .map(_.split(" ").map(Integer.parseInt(_)))
    .map(_.sum)
    .sorted
    .reverse
  
  val part1Solution = data.head
  val part2Solution = data.take(3).sum

  println(part1Solution)
  println(part2Solution)
```

1. Read input from file into list of each line in the file.
2. Join all the strings together with whitespace.
3. Numbers belonging to the same group are now separated by one space, and groups are separated by two.
4. Split on double space to create lists representing the numbers in each group (`Seq[Seq[String]]`).
5. Split each of the subsequences and parse the integers from the strings.
6. Sum each of the subsequences, creating a sequence of integers representing the total of each sequence.
7. Sort and reverse the list.
