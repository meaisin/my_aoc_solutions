class TreeMap(val data: List[List[Int]]):
  override def toString: String = data.map(_.mkString).mkString("\n")

  def get(x: Int, y: Int): Int =
    data(x)(y)

  def getRow(x: Int): List[Int] =
    data(x)

  def getCol(y: Int): List[Int] =
    data.transpose.apply(y)

  def getRowLeft(x: Int, y: Int): List[Int] =
    getRow(x).take(y).reverse

  def getRowRight(x: Int, y: Int): List[Int] =
    getRow(x).drop(y + 1)

  def getColUp(x: Int, y: Int): List[Int] =
    getCol(y).take(x).reverse

  def getColDown(x: Int, y: Int): List[Int] =
    getCol(y).drop(x + 1)

  def isPointVisible(x: Int, y: Int): Boolean =
    val height = data(x)(y)
    getColUp(x, y).dropWhile(_ < height).length == 0 ||
      getColDown(x, y).dropWhile(_ < height).length == 0 ||
      getRowLeft(x, y).dropWhile(_ < height).length == 0 ||
      getRowRight(x, y).dropWhile(_ < height).length == 0

  def scenicScore(x: Int, y: Int): Int =
    val lines = List(getColUp(x, y), getColDown(x, y), getRowLeft(x, y), getRowRight(x, y))
    val height = data(x)(y)

    val scores = lines.map(TreeMap.sightDistance(height, _))
    scores.foldLeft(1)(_ * _)

object TreeMap:
  def sightDistance(height: Int, line: List[Int]): Int =
    line.length match
      case 0 => 0
      case 1 => 1
      case _ => 
        val taken = line.takeWhile(_ < height).length
        val rem = line.dropWhile(_ < height).length
        if rem == 0 then
          taken
        else
          taken + 1


@main def main(arg: String): Unit =
  import scala.io.Source
  val data = Source
    .fromFile(arg)
    .getLines
    .toList
    .map(_.toList.map(_.toString).map(Integer.parseInt(_)))

  val treeMap = TreeMap(data)

  val visibleTrees = for
    i <- 0 to data.length - 1
    j <- 0 to data.head.length - 1
  yield
   treeMap.isPointVisible(i, j)

  val part1Solution = visibleTrees.filter(x => x).length

  val treeScores = for
    i <- 0 to data.length - 1
    j <- 0 to data.head.length - 1
  yield
    treeMap.scenicScore(i, j)

  val part2Solution = treeScores.toList.max

  println(part1Solution)
  println(part2Solution)
