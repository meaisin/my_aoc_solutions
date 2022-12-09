enum Direction:
  case Up
  case Down
  case Left
  case Right

class Rope:
  var headPos = (0, 0)
  var tailPos = (0, 0)

  private var lastHeadPos: Option[(Int, Int)] = None

  var headVisited: Map[(Int, Int), Int] = Map.empty[(Int, Int), Int]
  var tailVisited: Map[(Int, Int), Int] = Map.empty[(Int, Int), Int]

  private def posDelta(headPos: (Int, Int), tailPos: (Int, Int)): (Int, Int) =
    val (hX, hY) = headPos
    val (tX, tY) = tailPos
    (hX - tX, hY - tY)

  private def isTouching(headPos: (Int, Int), tailPos: (Int, Int)): Boolean =
    posDelta(headPos, tailPos) match
      case (0, 0) | (0, 1) | (1, 0) | (1, 1) => true
      case (-1, 0) | (0, -1) | (-1, -1) => true
      case (1, -1) | (-1, 1) => true
      case _ => false

    private def adjustTailPos(headPos: (Int, Int), tailPos: (Int, Int), lastHeadPos: Option[(Int, Int)]) =
      if isTouching(headPos, tailPos) then
        tailPos
      else 
        lastHeadPos match
          Some(pos) => pos
          None => throw new RuntimeException()

end Rope

@main def main(arg: String): Unit =
  import scala.io.Source

  val data = Source
    .fromFile(arg)
    .getLines
    .toList

  println(data.length)
