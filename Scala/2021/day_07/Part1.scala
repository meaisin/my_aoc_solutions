// Verified solution :)

@main def main(arg: String): Unit =
  import scala.io.Source
  val positions = Source
    .fromFile(arg)
    .getLines
    .toList
    .map(_.split(","))
    .flatten
    .map(Integer.parseInt(_))

  val minPosition = positions.min
  val maxPosition = positions.max

  val deltaLists = for i <- minPosition to maxPosition yield
    positions.map(x => (x - i).abs)

  val deltaSums = deltaLists.map(_.sum)

  println(deltaSums.min)
