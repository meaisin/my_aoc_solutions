// Verified solution :)

import scala.io.Source

@main def main(arg: String): Unit =
  val gammaRateBits = Source
    .fromFile(arg)
    .getLines
    .toList
    .map(explode(_).toList)
    .foldLeft(List.fill(12)(0))(combine)
    .map(x => if x >= 500 then 1 else 0)
  val epsilonRateBits = invert(gammaRateBits)

  val gammaRate = Integer.parseInt(gammaRateBits.mkString, 2)
  val epsilonRate = Integer.parseInt(epsilonRateBits.mkString, 2)

  println(gammaRate * epsilonRate)

// constant (every string is 12 bits)
def explode(binaryString: String) =
  for bit <- binaryString yield
    bit match
      case '0' => 0
      case '1' => 1

// O(n) but done n times, so O(n^2). Could improve by recursively 
// consuming each list at the same time, but this is good enough for now.
def combine(l1: List[Int], l2: List[Int]): List[Int] =
  l1.zip(l2).map((a, b) => a + b)

// O(n), done once
def invert(list: List[Int]): List[Int] =
  list.map(x => if x == 1 then 0 else 1)
