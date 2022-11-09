// Verified solution :)

import scala.io.Source

@main def main(arg: String): Unit =
  val result = produceSlidingMeasurements(Source
      .fromFile(arg)
      .getLines
      .toList
      .map(Integer.parseInt(_)))
    .sliding(2, 1)
    .map((x: List[Int]) => x(0) < x(1))
    .filter(x => x)
    .length
  println(result)


def produceSlidingMeasurements(linesOuter: List[Int]): List[Int] =
  require(linesOuter.length >= 3)
  def inner(lines: List[Int], result: List[Int]): List[Int] =
    lines match
      case List(x, y, z) => (x + y + z) :: result
      case xs => inner(lines.tail, lines.take(3).sum :: result)
  inner(linesOuter, List()).reverse
