// Verified solution :) (Both parts)

class CaveMap(val locationData: Array[Array[Int]]):
  class Point(val x: Int, val y: Int, val height: Int):
    override def toString(): String = s"[$x,$y]: $height"

    override def equals(that: Any): Boolean =
      that match
        case point: Point => point.x == x && point.y == y && point.height == height
        case _ => false

    override def hashCode(): Int =  31 * (x.##) + y.## + height.##

  val maximumX: Int = locationData.head.size - 1
  val maximumY: Int = locationData.size - 1

  val pointData = for
    j <- 0 to maximumX
    i <- 0 to maximumY
  yield
    Point(i, j, (locationData(i)(j)))

  private def adjacentPoints(point: Point): List[Point] =
    val units = List((1, 0), (0, 1), (-1, 0), (0, -1))
    units
      .map((a, b) => (point.x + a, point.y + b))
      .filter((a, b) => b >= 0 && b <= maximumX && a >= 0 && a <= maximumY)
      .map((a, b) => Point(a, b, locationData(a)(b)))

  private def adjacentPointsSansNines(points: List[Point]): List[Point] =
    points.flatMap(adjacentPoints(_)).filter(_.height < 9)

  private def basinPoints(point: Point): List[Point] =
    def inner(candidatePoints: List[Point], confirmedPoints: List[Point]): List[Point] =
      if candidatePoints.isEmpty then
        confirmedPoints
      else
        inner(adjacentPointsSansNines(candidatePoints).toSet.toList.filter(!confirmedPoints.contains(_)), candidatePoints concat confirmedPoints)
    inner(List(point), List())

  private val lowPoints: List[Point] =
    pointData
      .toList
      .filter(x => adjacentPoints(x)
        .map(_.height > x.height)
        .foldLeft(true)((acc, x) => acc && x))

  val part1Solution = lowPoints
    .map(_.height + 1)
    .sum

  val part2Solution = lowPoints
    .map(basinPoints(_))
    .map(_.length)
    .sorted
    .reverse
    .take(3)
    .product

  val basins = lowPoints.map(basinPoints(_))
  val basinSizes = basins.map(_.length).sorted.reverse

end CaveMap


@main def main(filename: String): Unit =
  val sourceData = getData(filename)

  val caveMap = CaveMap(sourceData)

  println(s"${caveMap.part1Solution}")

  println(s"${caveMap.part2Solution}")

end main

def getData(arg: String): Array[Array[Int]] =
  import scala.io.Source
  Source
    .fromFile(arg)
    .getLines
    .toList
    .map(_.toArray.map((x: Char) => Integer.parseInt(x.toString)))
    .toArray

end getData
