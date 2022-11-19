// Idea for solving part 2:
// Create Basin class which takes minimum points as basis
// Recursive function that adds points to basin
// Calculate basin size
// Map across generated basins to return basin sizes.
// Sort basins by basin size.
// Take 3.
// Return their product.

class Point(val x: Int, val y: Int, val height: Int, gAP: (Int, Int) => List[(Int, Int)]):
  val adjacentPoints = gAP(x, y)
  val riskLevel = height + 1
  override def toString(): String =
    s"[$x, $y]: $height"

@main def main(arg: String): Unit =
  import scala.io.Source

  val data = Source
    .fromFile(arg)
    .getLines
    .toList
    .map(_.map((x: Char) => Integer.parseInt(x.toString)).toArray)
    .toArray

  val points = for 
    i <- 0 to (data.size - 1)
    j <- 0 to (data.head.size - 1)
  yield
    Point(i, j, data(i)(j), genAdjacentPoints(data.size - 1, data.head.size - 1))

  points.foreach{x =>
    println(x)
    println(x.adjacentPoints)
  }

  val result = points
    .filter(x => x.adjacentPoints.map((a, b) => x.height < data(a)(b)).forall((x: Boolean) => x))
    .map(_.riskLevel)
    .sum

  println(result)

def genAdjacentPoints(xMax: Int, yMax: Int)(a: Int, b: Int): List[(Int, Int)] =
  val units = List((1, 0), (-1, 0), (0, 1), (0, -1))
  val adjacentPoints = units.map((x, y) => (a + x, b + y))
  adjacentPoints.filter((x, y) => x >= 0 && x <= xMax && y >= 0 && y <= yMax)
