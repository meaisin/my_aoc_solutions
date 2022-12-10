class Point(val x: Int, val y: Int):
  override def toString: String = s"[$x;$y]"

  override def equals(that: Any): Boolean =
    that match
      case p: Point => x == p.x && y == p.y
      case _ => false

  def toTuple: (Int, Int) =
    (this.x, this.y)

  def -(that: Point): Point =
    Point(this.x - that.x, this.y - that.y)

  def +(that: Point): Point =
    Point(this.x + that.x, this.y + that.y)

  def adjacent(that: Point): Boolean =
    val delta = this - that
    delta.toTuple match
      case (0, 0) => true
      case (1, 0) | (0, 1) | (1, 1) => true
      case (-1, 0) | (0, -1) | (-1, -1) => true
      case (-1, 1) | (1, -1) => true
      case _ => false

end Point

object Point:
  val origin: Point = Point(0, 0)

  def moveAlong(newHead: Point, 
    points: Array[Point]): Array[Point] = {
      val newPoints = points
      for
        i <- 0 to points.length.-(1)
      do
        println(s"$newPoints")
        if i == 0 then
          newPoints(i) = newHead
        else
          if newPoints(i) adjacent newPoints(i - 1) then
            ()
          else
            newPoints(i) = points(i - 1)
      newPoints.toArray
  }

end Point

class Rope(val length: Int = 2):
  require(length >= 2)
  import Point.*

  val points: Array[Point] = Array.fill(length)(origin)

end Rope
