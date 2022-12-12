// (Worry level of item, monkey ID to throw to)
type Throw = (Long, Int)

class Monkey(
  val id: Int,
  val initialItems: List[Long],
  val operation: (Long => Long),
  val test: Long,
  val trueThrowId: Int,
  val falseThrowId: Int
  ):
  
  override def toString: String = {
    s"Monkey $id\n\tStarting items: $initialItems\n" +
      s"\tOperation: $operation\n\tTest: $test\n" +
      s"\tIf true: throw to monkey $trueThrowId\n" +
      s"\tIf false: throw to monkey $falseThrowId\n"
  }

  private var monkeyBusiness: BigInt = BigInt(0)

  def mb: BigInt = monkeyBusiness

  private var items = initialItems

  private def inspectItem(item: Long): Long = {
    monkeyBusiness += 1;
    operation(item) / 3
  }

  private def inspectItem(item: Long, reducer: Long): Long = {
    monkeyBusiness += 1
    operation(item) % reducer
  }

  private def testItem(item: Long): Boolean = {
    item % test == 0
  }

  private def throwTo(item: Long): Int = {
    if testItem(item) then 
      trueThrowId 
    else 
      falseThrowId
  }

  def takeTurn(reducer: Option[Long]): List[Throw] = {
    val f = reducer match
      case Some(r) => (l: Long) => inspectItem(l, r)
      case _ => (l: Long) => inspectItem(l)
    val thrownItems = items
      .map(f)
      .map(x => (x, throwTo(x)))
    items = List()
    thrownItems
  }

  def receiveItem(item: Long): Unit = {
    items = items appended item
  }


end Monkey

object Monkey:
  def parseMId(string: String): Int = {
    string.split(" ").toList match
      case List("Monkey", id) => id.init.toInt
      case _ => throw new RuntimeException("Bad format!")
  }

  def parseMInitialItems(string: String): List[Long] = {
    def parseItems(items: String): List[Long] = {
      items.split(",").toList.map(_.toLong)
    }

    string.split(" ").toList match
      case "Starting" :: "items:" :: items => parseItems(items.mkString)
      case _ => throw new RuntimeException("Bad format!")
  }

  def parseMOperation(string: String): (Long => Long) = {
    def parseOper(oper: List[String]): (Long => Long) = {
      oper match
        case List("old", "*", "old") => ((x: Long) => x * x)
        case List("old", "*", n) => ((x: Long) => x * n.toLong)
        case List("old", "+", n) => ((x: Long) => x + n.toLong)
        case _ => throw new RuntimeException("Bad format!")
    }

    string.split(" ").toList match
      case "Operation:" :: "new" :: "=" :: oper => parseOper(oper)
      case _ => throw new RuntimeException("Bad format!")
  }

  def parseMTest(string: String): Long = {
    string.split(" ").toList.last.toLong
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

class Monkeys(val monkeyList: List[Monkey], val part2: Boolean):
  val reducer: Option[Long] = if part2 then
    Some(monkeyList.map(_.test).product) else None

  private def handleThrow(aThrow: Throw): Unit = {
    val (item, id) = aThrow
    monkeyList(id).receiveItem(item)
  }

  private def handleThrows(throws: List[Throw]): Unit = {
    throws.foreach{x =>
      handleThrow(x)
    }
  }

  private def handleTurn: Unit = {
    for
      monkey <- monkeyList
    do
      handleThrows(monkey.takeTurn(reducer))
  }

  def handleTurns(n: Int): Unit = {
    for
      i <- 0 to (n - 1)
    do
      handleTurn
  }

  def solution: BigInt = {
    monkeyList.map(_.mb).sorted.reverse.take(2).product
  }

end Monkeys

@main def main(arg: String): Unit =
  import scala.io.Source

  val data = Source
    .fromFile(arg)
    .getLines
    .grouped(7)
    .map(_.map(_.trim).filter(_ != "").toList)
    .toList

  val monkeys1 = Monkeys(data
    .map(Monkey.parseMonkey(_)), false)

  monkeys1.handleTurns(20)

  println(monkeys1.solution)

  val monkeys2 = Monkeys(data
    .map(Monkey.parseMonkey(_)), true)

  monkeys2.handleTurns(10000)

  println(monkeys2.solution)
