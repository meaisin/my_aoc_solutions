enum Position:
  case Leftmost
  case Rightmost
  case Neither

// Erroneous adjacent positions:
// Rightmost bottom corner: 1 erroneous position.
// Leftmost bottom corner: 3 erroneous positions.
// Rightmost: 3 erroneous positions. (Those whos pos % 10 == 0)
// Leftmost: 3 erroneous positions. (Those whose pos % 10 == 9)
//
//

class Point(val pos: Int, val energy: Int):
  override def toString: String =
    s"[$pos]: $energy"

  val specialFilter = 
    (pos % 10) match
      case 0 => (x: Int) => x % 10 != 9
      case 9 => (x: Int) => x % 10 != 0
      case _ => (x: Int) => true

  val adjacentPoints: List[Int] =
    List(1, -1, 9, -9, 10, -10, 11, -11)
      .map(pos + _)
      .filter(x => x >= 0 && x <= 99)
      .filter(specialFilter(_))

@main def main(arg: String): Unit =
  import scala.io.Source

  val data = Source
    .fromFile(arg)
    .getLines
    .toList
    .flatMap(_.toList)
    .map((x: Char) => Integer.parseInt(x.toString))

  val points = for
    i <- 0 to data.length - 1
  yield
    Point(i, data(i))

  points
    .filter(x => x.pos % 10 == 0 || x.pos % 10 == 9)
    .foreach{x =>
    println(s"$x: ${x.adjacentPoints}")
  }
