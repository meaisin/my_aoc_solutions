// Verified solution :)

import scala.io.Source

@main def main(arg: String): Unit =
  val (x, y) = Source
    .fromFile(arg)
    .getLines
    .toList
    .map(_.split(" "))
    .map(list => (list(0), Integer.parseInt(list(1))))
    .map(toDirectionVector(_))
    .foldLeft((0, 0))((x, y) => (x._1 + y._1, x._2 + y._2))
  println(x * y)

def toDirectionVector(pair: (String, Int)): (Int, Int) =
  pair match
    case ("forward", x) => (x, 0)
    case ("up", x) => (0, -1 * x)
    case ("down", x) => (0, x)
