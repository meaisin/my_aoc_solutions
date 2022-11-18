// Verified solution :)

@main def main(arg: String): Unit =
  import scala.io.Source
  val file = Source
    .fromFile(arg)
    .getLines
    .toList
    .map(_.split(","))
    .flatten
    .map(Integer.parseInt(_))

  val data = listToMap(file)
  println(data)
  val result = solve(256, data)
  println(result.valuesIterator.toList.sum)

def listToMap(list: List[Int]): Map[Int, BigInt] =
  def inner(list: List[Int], map: Map[Int, BigInt]): Map[Int, BigInt] =
    list match
      case List() => map
      case x :: xs => inner(xs, map + ((x, map(x) + 1): (Int, BigInt)))
  inner(list, Map(0 -> BigInt(0), 1 -> BigInt(0), 2 -> BigInt(0), 3 -> BigInt(0), 4 -> BigInt(0), 5 -> BigInt(0), 6 -> BigInt(0), 7 -> BigInt(0), 8 -> BigInt(0)))

def iterate(map: Map[Int, BigInt]): Map[Int, BigInt] =
  val zeros = map(0)
  val ones = map(1)
  val twos = map(2)
  val threes = map(3)
  val fours = map(4)
  val fives = map(5)
  val sixes = map(6)
  val sevens = map(7)
  val eights = map(8)

  val newZeros = ones
  val newOnes = twos
  val newTwos = threes
  val newThrees = fours
  val newFours = fives
  val newFives = sixes
  val newSixes = sevens + zeros
  val newSevens = eights
  val newEights = zeros

  Map(
    0 -> newZeros,
    1 -> newOnes,
    2 -> newTwos,
    3 -> newThrees,
    4 -> newFours,
    5 -> newFives,
    6 -> newSixes,
    7 -> newSevens,
    8 -> newEights
    )

def solve(counter: Int, map: Map[Int, BigInt]): Map[Int, BigInt] =
  def inner(counter: Int, map: Map[Int, BigInt]): Map[Int, BigInt] =
    if counter == 0 then
      map
    else
      inner(
        counter - 1,
        iterate(map)
        )
  inner(counter, map)
