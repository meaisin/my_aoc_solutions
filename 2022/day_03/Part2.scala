@main def main(arg: String): Unit =
  import scala.io.Source
  val data = Source
    .fromFile(arg)
    .getLines
    .toList
    .grouped(3)
    .toList
    .map(_.map(_.toSet))
    .map(_.reduceLeft((x, y) => x intersect y))
    .flatten
    .map{x =>
      if x.isUpper then (x.toInt - 38) else (x.toInt - 96)
    }
    .sum

    println(data)
