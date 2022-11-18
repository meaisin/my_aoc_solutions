// Verified solution for both parts :)

import scala.io.Source

class Point(val x: Int, val y: Int):
  override def toString(): String =
    s"[$x, $y]"

  override def equals(that: Any): Boolean =
    that match
      case that: Point => this.x == that.x && this.y == that.y
      case _ => false

  override def hashCode(): Int =
    31 * (this.x.##) + this.y.##

  def -(that: Point): Point =
    Point(this.x - that.x, this.y - that.y)

  def +(that: Point): Point =
    Point(this.x + that.x, this.y + that.y)

  /*def diagonalize(that: Point): List[Point] =
    val unit = (this.x < that.x, this.y < that.y) match
      case (true, true) => Point(1, 1)
      case (true, false) => Point(1, -1)
      case (false, true) => Point(-1, 1)
      case (false, false) => Point(-1, -1)
    def inner(list: List[Point], point: Point): List[Point] =
      println(s"List: $list, Point: $point")
      val newPointList = (point + unit) :: list
      if that == newPointList then
        newPointList
      else
        inner(newPointList, point + unit)
    inner(List(), this)
    */
  def diagonalize(that: Point): List[Point] =
    val xRange = if this.x < that.x then (this.x to that.x) else (this.x to that.x by -1)
    val yRange = if this.y < that.y then (this.y to that.y) else (this.y to that.y by -1)
    xRange.toList.zip(yRange.toList).map(x => Point(x._1, x._2))


class Line(val point1: Point, val point2: Point):
  private val delta = point1 - point2
  val pointsOnLine = if (delta.x.abs == delta.y.abs) then
    point1.diagonalize(point2)
  else
      (delta.x, delta.y) match
      case (0, yD) => {
        val (lowerY, upperY) = if point1.y < point2.y then (point1.y, point2.y) else (point2.y, point1.y)
        (lowerY to upperY).toList.map(y => Point(point1.x, y))
      }
      case (xD, 0) => {
        val (lowerX, upperX) = if point1.x < point2.x then (point1.x, point2.x) else (point2.x, point1.x)
        (lowerX to upperX).toList.map(x => Point(x, point1.y))
      }

  override def toString(): String =
    s"$point1 -> $point2"

object Line:
  def parseLine(line: String): Option[Line] =
    import scala.util.matching.Regex

    val lineRegex: Regex = """(-?\d+),(-?\d+) -> (-?\d+),(-?\d+)""".r

    val (x1, y1, x2, y2) = line match
      case lineRegex(x1, y1, x2, y2) => (x1, y1, x2, y2)
      case _ => throw new RuntimeException("Unexpected format.")

    val point1 = Point(Integer.parseInt(x1), Integer.parseInt(y1))
    val point2 = Point(Integer.parseInt(x2), Integer.parseInt(y2))

    val delta = point1 - point2

    if (delta.x.abs == delta.y.abs) then 
      Some(Line(point1, point2))
    else
      (delta.x, delta.y) match
        case (0, _) => Some(Line(point1, point2))
        case (_, 0) => Some(Line(point1, point2))
        case _ => None

  def returnLineOverlapMap(list: List[List[Point]]): Map[Point, Int] =
    def inner(list: List[Point], map: Map[Point, Int]): Map[Point, Int] =
      list match
        case List() => map
        case x :: xs => inner(xs, map.updatedWith(x){
          case Some(value) => Some(value + 1)
          case None => Some(1)
        })
    inner(list.flatten, Map())


@main def main(arg: String): Unit =
  val data = Source
    .fromFile(arg)
    .getLines
    .toList
    .map(Line.parseLine(_))
    .filter{
      _ match
        case Some(_) => true
        case None => false
    }
    .map{
      _ match
        case Some(x) => x
        case None => throw new RuntimeException("Impossible")
    }

  val dataPoints = data.map(_.pointsOnLine)

  val lineOverlaps = Line.returnLineOverlapMap(dataPoints).filter((k, v) => v >= 2)

  println(lineOverlaps.size)
