type Tuple = (Int, Int)

@main def main(arg: String): Unit = {
  import scala.io.Source

  val input = Source
    .fromFile(arg)
    .getLines
    .toList

  process(input)
}

def p2p(p1: Tuple, p2: Tuple): List[Tuple] = {
  val (p1x, p1y) = p1
  val (p2x, p2y) = p2

  val xRange = if p1x < p2x then (p1x to p2x) else (p2x to p1x)
  val yRange = if p1y < p2y then (p1y to p2y) else (p2y to p1y)

  val output = for
    i <- xRange
    j <- yRange
  yield
    (i, j)
  output.toList
}

def process(strings: List[String]): Set[Tuple] = {
  val output = strings
    .map(_
      .split(" -> ")
      .toList
      .sliding(2)
      .toList
      .map(_
        .map(_
          .split(","))
        .map(x => (x(0), x(1))))
      .flatten
      .distinct
      .map(x => (x._1.toInt, x._2.toInt))
      .sliding(2)
      .toList
      .map(x => (x(0), x(1)))
      .map(p2p(_, _).toSet)
      .toSet.flatten)
    .toSet.flatten

  output
}
