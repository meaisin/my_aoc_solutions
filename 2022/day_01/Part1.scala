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
