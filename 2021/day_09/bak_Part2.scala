// Idea for solving part 2:
// Create Basin class which takes minimum points as basis
// Recursive function that adds points to basin
// Calculate basin size
// Map across generated basins to return basin sizes.
// Sort basins by basin size.
// Take 3.
// Return their product.


// Generate adjacent points for a minimum point.
// Filter points for being > minimum && != 9.
// Add points to confirmed points.
// Generate new adjacent points by mapping over points with getAdjacentPoints and flattening (flatMap?)


class Point(val x: Int, val y: Int, val height: Int, gAP: (Int, Int) => List[(Int, Int)]):
  val adjacentPoints = gAP(x, y)
  val riskLevel = height + 1
  override def toString(): String =
    s"[$x, $y]: $height"

class Basin(val minima: Point, gAP: (Int, Int) => List[(Int, Int)]):
  def getLocations(): List[(Int, Int)] =
    def inner(possibleLocations: List[(Int, Int)], locations: List[(Int, Int)]): List[(Int, Int)] =
      println(s"$possibleLocations\n$locations")
      if possibleLocations.isEmpty then
        locations
      else
        inner(possibleLocations.map((x, y) => gAP(x, y)).flatten, possibleLocations concat locations)
    inner(List((minima.x, minima.y)), List())

  val locations = getLocations()

  val basinSize = locations.length

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

  val lowPoints = points
    .filter(x => x.adjacentPoints.map((a, b) => x.height < data(a)(b)).forall((x: Boolean) => x))
  
  val result = lowPoints
    .map(_.riskLevel)
    .sum

  println(result)

  val basins = lowPoints
    .map(Basin(_, genAdjacentPoints(data.size - 1, data.head.size - 1)))

  basins.foreach{x =>
    println(x.basinSize)
  }

def genAdjacentPoints(xMax: Int, yMax: Int)(a: Int, b: Int): List[(Int, Int)] =
  val units = List((1, 0), (-1, 0), (0, 1), (0, -1))
  val adjacentPoints = units.map((x, y) => (a + x, b + y))
  adjacentPoints.filter((x, y) => x >= 0 && x <= xMax && y >= 0 && y <= yMax)
