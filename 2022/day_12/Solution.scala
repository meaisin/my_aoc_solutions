class Graph(val initializer: List[String]):
  private val temp1 = initializer
    .map(_.toList)

  val heights = temp1
    .map(_
      .map{x =>
        x match
          case 'S' => 0
          case 'E' => 'z'.toInt - 'a'.toInt
          case c => c.toInt - 'a'.toInt
    })

  val isReachable = Graph.isReachable(heights)

  val prettyMap: String = {
    val string = for
      i <- 0 to (heights.length -1)
      j <- 0 to (heights.head.length - 1)
    yield
      if isReachable(i, j) then
        "."
      else
        println(s"[$i;$j]: ${heights(i)(j)} unreachable")
        "#"
    string.toList.grouped(heights.head.length).map(_.mkString).mkString("\n")
  }

end Graph

object Graph:
  def isReachable(data: List[List[Int]])(i: Int, j: Int): Boolean = {
    val maxI = data.length - 1
    val maxJ = data.head.length - 1
    val height = data(i)(j)

    val unitTuples = List(
      (1, 0), (0, 1), (-1, 0), (0, -1)
      )

    unitTuples
      .map(x => (x._1 + i, x._2 + j))
      .filter(x => x._1 >= 0 && x._1 <= maxI && x._2 >= 0 && x._2 <= maxJ)
      .map(x => data(x._1)(x._2))
      .map(x => height - x)
      .filter(_ >= -1)
      .length != 0
  }
end Graph

@main def main(arg: String): Unit = {
  import scala.io.Source

  val input = Source
    .fromFile(arg)
    .getLines
    .toList

  val graph: Graph = Graph(input)
  println(graph.prettyMap)
}
