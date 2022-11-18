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

  val data = solve(256, listToMap(file))
  println(data.valuesIterator.toList.sum)

def listToMap(list: List[Int]): Map[Int, BigInt] =
  def inner(list: List[Int], map: Map[Int, BigInt]): Map[Int, BigInt] =
    list match
      case List() => map
      case x :: xs => inner(xs, map + ((x, map(x) + 1): (Int, BigInt)))
  inner(list, Map(0 -> BigInt(0), 1 -> BigInt(0), 2 -> BigInt(0), 3 -> BigInt(0), 4 -> BigInt(0), 5 -> BigInt(0), 6 -> BigInt(0), 7 -> BigInt(0), 8 -> BigInt(0)))

def iterate(map: Map[Int, BigInt]): Map[Int, BigInt] =
  Map(
    0 -> map(1),
    1 -> map(2),
    2 -> map(3),
    3 -> map(4),
    4 -> map(5),
    5 -> map(6),
    6 -> (map(7) + map(0)),
    7 -> map(8),
    8 -> map(0)
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
