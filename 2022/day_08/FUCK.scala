def sightlineGen(data: List[List[Int]])(x: Int, y: Int): List[List[Int]] =
  val (beforeTemp, afterTemp) = data(x).splitAt(x)
  val (aboveTemp, belowTemp) = data.transpose.apply(y).splitAt(y)
  val (before, above) = (beforeTemp.reverse, aboveTemp.reverse)
  val (after, below) = (afterTemp.tail, belowTemp.tail)
  val list = List(before, after, above, below)
  println(list)
  list

def part1(data: List[List[Int]])(x: Int, y: Int): Boolean =
  sightlineGen(data)(x, y)
    .map(_.dropWhile(_ < data(x)(y)))
    .map(_.length == 0)
    .exists(x => x )

@main def main(arg: String): Unit =
  import scala.io.Source

  val data = Source
    .fromFile(arg)
    .getLines
    .toList
    .map(_.toList.map(x => Integer.parseInt(x.toString)))

  val xUB = data.length - 1
  val yUB = data.head.length - 1

  val part1Fun = part1(data)

  val visibleTrees = for
    i <- 0 to xUB
    j <- 0 to yUB
  yield
    if part1Fun(i, j) then 1 else 0

  println(visibleTrees.toList.grouped(5).map(_.mkString).mkString("\n"))
  val part1Solution = visibleTrees.sum

  println(part1Solution)
