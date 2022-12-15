class SB(val rawData: String):
  val ((sx, sy), (bx, by)) = {
    import scala.util.matching.Regex

    val regex: Regex = 
      """Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)""".r

    rawData match
      case regex(x1, y1, x2, y2) => ((x1.toLong, y1.toLong), (x2.toLong, y2.toLong))
      case _ => throw new RuntimeException(s"Bad format! $rawData")
  }
  val manhattanRange = (sx - bx).abs + (sy - by).abs

  val minX = sx - manhattanRange
  val maxX = sx + manhattanRange

  def withinRange(xy: (Long, Long)): Boolean = {
    ((sx - xy._1).abs + (sy - xy._2).abs) <= manhattanRange
  }

  def noBeacon(xy: (Long, Long)): Boolean = {
    withinRange(xy) && xy != (bx, by)
  }

  override def toString: String = 
    s"Sensor: [$sx, $sy], Beacon: [$bx,$by], Manhattan range: $manhattanRange"
end SB

object SB:
  def min(data: List[SB]): Long = {
    val output = data
      .map(_.minX)
      .sorted
    output.head
  }

  def max(data: List[SB]): Long = {
    val output = data
      .map(_.maxX)
      .sorted
      .reverse
    output.head
  }

  def withinAny(data: List[SB])(point: (Long, Long)): Boolean =
    data.map(x => if x.noBeacon(point) then 1 else 0).sum >= 1

  def part1Solve(data: List[SB])(yLevel: Long): Long = {
    val start = min(data)
    val end = max(data)
    val within = withinAny(data)

    val points = 
      for
        i <- start to end
        if within((i, yLevel))
      yield
        (i, yLevel)

    points.toList.length
  }
end SB

@main def main(arg: String): Unit = {
  import scala.io.Source

  val input = Source
    .fromFile(arg)
    .getLines
    .toList

  val data = input
    .map(SB(_))

  val test = SB.part1Solve(data)(10)
  println(test)
}
