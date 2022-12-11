// (Worry level of item, monkey ID to throw to)
type Throw = (Int, Int)

class Monkey(
  val id: Int,
  val initialItems: List[Int],
  val operation: (Int => Int),
  val test: (Int => Boolean),
  val trueThrowId: Int,
  val falseThrowId: Int
  ):
  
  override def toString: String = {
    s"Monkey $id\n\tStarting items: $initialItems\n" +
      s"\tOperation: $operation\n\tTest: $test\n" +
      s"\tIf true: throw to monkey $trueThrowId\n" +
      s"\tIf false: throw to monkey $falseThrowId\n"
  }

  private var items = initialItems

  private def inspectItem(item: Int): Int = {
    operation(item) / 3
  }

  private def testItem(item: Int): Boolean = {
    test(item)
  }

  private def throwTo(item: Int): Int = {
    if testItem(item) then 
      trueThrowId 
    else 
      falseThrowId
  }

  def takeTurn: List[Throw] = {
    val thrownItems = items
      .map(inspectItem)
      .map(x => (x, throwTo(x)))
    items = List()
    thrownItems
  }

  def receiveItem(item: Int): Unit = {
    items = items appended item
  }


end Monkey

object Monkey:
  def parseMId(string: String): Int = {
    string.split(" ").toList match
      case List("Monkey", id) => id.init.toInt
      case _ => throw new RuntimeException("Bad format!")
  }

  def parseMInitialItems(string: String): List[Int] = {
    def parseItems(items: String): List[Int] = {
      items.split(",").toList.map(_.toInt)
    }

    string.split(" ").toList match
      case "Starting" :: "items:" :: items => parseItems(items.mkString)
      case _ => throw new RuntimeException("Bad format!")
  }

  def parseMOperation(string: String): (Int => Int) = {
    def parseOper(oper: List[String]): (Int => Int) = {
      oper match
        case List("old", "*", "old") => ((x: Int) => x * x)
        case List("old", "*", n) => ((x: Int) => x * n.toInt)
        case List("old", "+", n) => ((x: Int) => x + n.toInt)
        case _ => throw new RuntimeException("Bad format!")
    }

    string.split(" ").toList match
      case "Operation:" :: "new" :: "=" :: oper => parseOper(oper)
      case _ => throw new RuntimeException("Bad format!")
  }

  def parseMTest(string: String): (Int => Boolean) = {
    string.split(" ").toList match
      case List("Test:", "divisible", "by", n) => 
        ((x: Int) => x % n.toInt == 0)
      case _ => throw new RuntimeException("Bad format!")
  }

  def parseMThrowId(string: String): Int = {
    string.split(" ").toList match
      case List("If", _, "throw", "to", "monkey", id) => id.toInt
      case _ => throw new RuntimeException("Bad format!")
  }

  def parseMonkey(info: List[String]): Monkey = {
    require(info.length == 6)

    Monkey(
      parseMId(info(0)),
      parseMInitialItems(info(1)),
      parseMOperation(info(2)),
      parseMTest(info(3)),
      parseMThrowId(info(4)),
      parseMThrowId(info(5))
      )
  }

end Monkey

@main def main(arg: String): Unit =
  import scala.io.Source

  val data = Source
    .fromFile(arg)
    .getLines
    .grouped(7)
    .map(_.map(_.trim).filter(_ != "").toList)
    .toList

  val monkeys = data
    .map(Monkey.parseMonkey(_))

  println(monkeys)

  println(monkeys.head.takeTurn)
