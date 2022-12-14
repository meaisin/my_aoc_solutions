class Rope:
  var headPos = (0, 0)
  var tailPos = (0, 0)

  private var lastHeadPos: Option[(Int, Int)] = None

  private var tailVisited: Map[(Int, Int), Int] = Map((0, 0) -> 1)

  def moveHeadBy(dir: Char, by: Int): Unit =
    if by == 0 then
      ()
    else
      moveHead(dir)
      moveHeadBy(dir, by - 1)

  def uniqueTailPositions: Int =
    tailVisited.size

  private def moveHead(dir: Char): Unit =
    val (hX, hY) = headPos
    lastHeadPos = Some(headPos)
    headPos = dir match
      case 'U' => (hX, hY + 1)
      case 'D' => (hX, hY - 1)
      case 'L' => (hX - 1, hY)
      case 'R' => (hX + 1, hY)
    moveTail


  private def moveTail: Unit =
    if isTouching then
      ()
    else
      tailPos = lastHeadPos match
        case Some(pos) => pos
        case None => throw new RuntimeException("No previous head position for tail to move to.")
      tailVisited = tailVisited + (tailPos -> tailVisited.getOrElse(tailPos, 0).+(1))


  private def posDelta: (Int, Int) =
    val (hX, hY) = headPos
    val (tX, tY) = tailPos
    (hX - tX, hY - tY)

  private def isTouching: Boolean =
    posDelta match
      case (0, 0) | (0, 1) | (1, 0) | (1, 1) => true
      case (-1, 0) | (0, -1) | (-1, -1) => true
      case (1, -1) | (-1, 1) => true
      case _ => false

end Rope

@main def main(arg: String): Unit =
  import scala.io.Source

  val data = Source
    .fromFile(arg)
    .getLines
    .toList

  val steps = data
    .map(x => (x.head, Integer.parseInt(x.drop(2))))

  val rope: Rope = Rope()

  steps.foreach{x =>
    rope.moveHeadBy(x._1, x._2)
  }

  println(rope.uniqueTailPositions)
