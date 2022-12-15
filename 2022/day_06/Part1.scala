def solve(data: String)(window: Int): Int =
  data
    .sliding(window)
    .zip(window to data.length)
    .toList
    .map(x => (x._1.toSet.size, x._2))
    .dropWhile(x => x._1 != window)
    .head
    ._2

@main def main(arg: String): Unit =
  import scala.io.Source

  val data = Source
    .fromFile(arg)
    .getLines
    .mkString
    
  val partialSolution = solve(data)
  val part1Solution = partialSolution(4)
  val part2Solution = partialSolution(14)

  println(part1Solution)
  println(part2Solution)
