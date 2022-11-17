import scala.io.Source

class Point(val x: Int, val y: Int):
  override def toString(): String =
    s"[$x, $y]"

  def -(that: Point): Point =
    Point(this.x - that.x, this.y - that.y)

class Line(val point1: Point, val point2: Point):
  private val delta = point1 - point2
  val pointsOnLine = (delta.x, delta.y) match
    case (0, yD) => (point1.y to point2.y).toList.map(y => Point(point1.x, y))
    case (xD, 0) => (point1.x to point2.x).toList.map(x => Point(x, point1.y))

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
    (delta.x, delta.y) match
      case (0, _) => Some(Line(point1, point2))
      case (_, 0) => Some(Line(point1, point2))
      case _ => None


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

  data.foreach(println)
