// Verified solution :)

import scala.io.Source

@main def main(arg: String): Unit =
  val result = Source
    .fromFile(arg)
    .getLines
    .toList
    .map(Integer.parseInt(_))
    .sliding(2, 1)
    .map(x => x(0) < x(1))
    .filter((x: Boolean) => x)
    .length
  println(result)
