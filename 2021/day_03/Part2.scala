// Verified solution :)

@main def main(arg: String): Unit =
  import scala.io.Source
  val data = Source
    .fromFile(arg)
    .getLines
    .toList
    .map(_.toList.map(f(_)))
  val oxygenRating = Integer.parseInt(solve(data, true).mkString, 2)
  val co2Rating = Integer.parseInt(solve(data, false).mkString, 2)
  println(oxygenRating * co2Rating)

def f(c: Char): Int =
  require(c == '1' || c == '0')
  if c == '1' then 1 else 0

def getDiscriminant(list: List[List[Int]], position: Int): Int =
  require(position < list.head.length)
  if list.map(x => x(position)).sum * 2 >= list.length then 1 else 0

def refine(list: List[List[Int]], position: Int, value: Int): List[List[Int]] =
  require(position < list.head.length && (value == 1 || value == 0))
  list.filter(x => x(position) == value)

def iterate(list: List[List[Int]], position: Int, mostCommon: Boolean): List[List[Int]] =
  val mean = getDiscriminant(list, position)
  val antiMean = if mean == 1 then 0 else 1
  refine(list, position, if mostCommon then mean else antiMean)

def solve(list: List[List[Int]], mostCommon: Boolean): List[Int] =
  def inner(list: List[List[Int]], position: Int): List[Int] =
    iterate(list, position, mostCommon) match
      case List() => throw new RuntimeException("bruh")
      case List(x) => x
      case newList => inner(newList, position + 1)
  inner(list, 0)
