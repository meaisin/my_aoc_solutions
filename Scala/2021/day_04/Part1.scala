// Verified solution :)
// (Fulfills part 1 and part 2 simultaneously)

class BingoTable(val numbers: List[Int]):
  require(numbers.length == 25)

  val rows = numbers.grouped(5).toList
  val columns = rows.transpose

  var lines = rows concat columns

  var calledOut: List[Int] = List()

  var finalCallout: Option[Int] = None

  var callouts: Int = 0

  def isBingo(): Boolean = lines.exists(_ == List())

  def countUnmarked(): Int = lines.map(_.length).sum

  def handleCallout(number: Int) =
    val previousCount = countUnmarked()
    lines = lines.map(_.filter(_ != number))
    (countUnmarked() != previousCount, isBingo()) match
      case (true, true) => finalCallout = Some(number); calledOut = number :: calledOut
      case (true, false) => calledOut = number :: calledOut
      case (_, _) => ()
    callouts = callouts + 1

  def sumOfUnmarked(): Int = numbers.filter(!calledOut.contains(_)).sum

  def winningCalculation(): Option[Int] =
    finalCallout match
      case None => None
      case Some(x) => Some(sumOfUnmarked() * x)

  def handleCalloutList(numbers: List[Int]): Unit =
    def inner(list: List[Int], won: Boolean): Unit =
      (list, won) match
        case (_, true) => ()
        case (x :: xs, false) => handleCallout(x); inner(xs, isBingo())
        case (_, _) => ()
    inner(numbers, isBingo())

@main def main(arg: String): Unit =
  import scala.io.Source

  val textData = Source
    .fromFile(arg)
    .getLines
    .toList
  
  val textHead = textData.head
  val textBody = textData.tail.filter(_ != "")

  val callouts = textHead.split(",").filter(_ != "").filter(_ != " ").map(Integer.parseInt(_)).toList

  val cards = textBody
    .filter(_ != "")
    .map(_.split(" ").filter(_ != "").map(Integer.parseInt(_)))
    .map(_.toList)
    .flatten
    .grouped(25)
    .toList
    .map(BingoTable.apply(_))
    
  cards.foreach(_.handleCalloutList(callouts))

  val data = cards
    .map(x => (x.callouts, x.winningCalculation()))
    .sorted

  println(data)
