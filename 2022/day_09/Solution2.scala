type Point = (Int, Int)

class Rope(val totalLength: Int = 2):
  require(totalLength >= 2)
  private val origin: Point = (0, 0)
  private var headPos = origin
  
  private val tailLength = totalLength - 1
  private var tail: Array[Point] = Array.fill(tailLength)(origin)

  private def neck: Point = tail(0)
  private def end: Point = tail(tailLength - 1)

  var pointsVisited: Set[Point] = Set(origin)

  def uniqueLocations: Int =
    pointsVisited.size

  def moveBy(dir: Char, by: Int): Unit  =
    if by == 0 then
      ()
    else
      move(dir)
      moveBy(dir, by - 1)

  private def move(dir: Char): Unit =
    val (x, y) = headPos
    val newPos = dir match
      case 'U' => (x, y + 1)
      case 'D' => (x, y - 1)
      case 'L' => (x - 1, y)
      case 'R' => (x + 1, y)
    if isContacting(newPos, neck) then
      headPos = newPos
    else
      updateTail(headPos)
      headPos = newPos
      
  private def updateTail(pos: Point): Unit =
    tail = moveAlong(pos, tail)
    visited(tail(tailLength - 1))

  private def visited(pos: Point): Unit =
    pointsVisited = pointsVisited + pos

  private def isContacting(pos1: Point, pos2: Point): Boolean =
    val (x1, y1) = pos1
    val (x2, y2) = pos2

    (x1 - x2, y1 - y2) match
      case (0, 0) => true
      case (0, 1) | (1, 0) | (1, 1) => true
      case (0, -1) | (-1, 0) | (-1, -1) => true
      case (1, -1) | (-1, 1) => true
      case _ => false

  private def moveAlong(newPoint: Point, array: Array[Point]): Array[Point] =
    if array.size == 1 then
      Array(newPoint)
    else
      val newArray = for
        i <- 0 to (array.size - 1)
      yield
        if i == 0 then
          newPoint
        else
          array(i - 1)
      newArray.toArray

end Rope

@main def main(arg: String): Unit =
  import scala.io.Source

  val data = Source
    .fromFile(arg)
    .getLines
    .toList

  val steps = data
    .map(x => (x.head, Integer.parseInt(x.drop(2))))

  val rope1: Rope = Rope(2)

  steps.foreach{x =>
    rope1.moveBy(x._1, x._2)
  }

  val part1Solution = rope1.uniqueLocations
  println(part1Solution)
  println(rope1.pointsVisited.toList)

  val rope2: Rope = Rope(10)

  steps.foreach{x =>
    rope2.moveBy(x._1, x._2)
  }

  val part2Solution = rope2.uniqueLocations
  println(part2Solution)
