// Verified solution :)

import scala.io.Source

@main def main(arg: String): Unit =
  val (x, y, _) = Source
    .fromFile(arg)
    .getLines
    .toList
    .map(_.split(" "))
    .map(list => (list(0), Integer.parseInt(list(1))))
    .foldLeft((0, 0, 0))(compute)
  println(x * y)

def compute(prev: (Int, Int, Int), next: (String, Int)): (Int, Int, Int) =
  val (horizontal, depth, aim) = prev
  val (direction, amount) = next
  direction match
    case "forward" => (horizontal + amount, depth + (amount * aim), aim)
    case "up" => (horizontal, depth, aim - amount)
    case "down" => (horizontal, depth, aim + amount)
