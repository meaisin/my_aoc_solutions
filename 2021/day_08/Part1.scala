@main def main(arg: String): Unit =
  import scala.io.Source

  val data = Source
    .fromFile(arg)
    .getLines
    .toList
    .map(_.split(" ").dropWhile(_ != "|").tail.map(_.length).filter(List(2, 3, 4, 7).contains(_)))
    .map(_.length)
    .sum

  println(data)
