class CaveMap(val locationData: Array[Array[Int]]):
  class Point(val x: Int, val y: Int, val height: Int):
    override def toString(): String = s"[$x,$y]: $height"

  val maximumX: Int = locationData.head.size - 1
  val maximumY: Int = locationData.size - 1

  val pointData = for
    i <- 0 to maximumX
    j <- 0 to maximumY
  yield
    Point(i, j, (locationData(i)(j)))

  def adjacentPoints(point: Point): List[Point] =
    val units = List((1, 0), (0, 1), (-1, 0), (0, -1))
    units
      .map((a, b) => (point.x + a, point.y + b))
      .filter((a, b) => a >= 0 && a <= maximumX && b >= 0 && b <= maximumY)
      .map((a, b) => Point(a, b, locationData(a)(b)))

  val lowPoints: List[Point] =
    pointData
      .toList
      .filter(x => adjacentPoints(x)
        .map(_.height > x.height)
        .foldLeft(true)((acc, x) => acc && x))


@main def main(filename: String): Unit =
  import scala.io.Source

  val sourceData = Source
    .fromFile(filename)
    .getLines
    .toList
    .map(_.toArray.map((x: Char) => Integer.parseInt(x.toString)))
    .toArray

  val caveMap = CaveMap(sourceData)

  println(s"${caveMap.pointData}")
  println(s"${caveMap.maximumX}, ${caveMap.maximumY}")
  println(s"${caveMap.pointData.size}")
  println(s"${caveMap.lowPoints.map(_.height).map(_ + 1).sum}")

end main
