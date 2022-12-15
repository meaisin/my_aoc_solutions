class Crates(initialState: List[List[String]]):
  val crateStacks = initialState.toArray

  override def toString: String =
    crateStacks.map(_.mkString(" ")).mkString("\n")

  def moveCrates(quantity: Int, from: Int, to: Int): Unit =
    if quantity == 0 then
      ()
    else
      val crate = crateStacks(from - 1).head
      crateStacks(from - 1) = crateStacks(from - 1).tail
      crateStacks(to - 1) = crate :: crateStacks(to - 1)
      moveCrates(quantity - 1, from, to)

  def moveCratesTogether(quantity: Int, from: Int, to: Int): Unit =
    val stack = crateStacks(from - 1).take(quantity)
    crateStacks(from - 1) = crateStacks(from - 1).drop(quantity)
    crateStacks(to - 1) = stack concat crateStacks(to - 1)

  val topCrates: String =
    crateStacks.toList.map(_.head).mkString

object Crates:
  def parseInstruction(instruction: String): (Int, Int, Int) =
    import scala.util.matching.Regex
    val instructionRegex = "move ([0-9]+) from ([0-9]+) to ([0-9]+)".r
    instruction match
      case instructionRegex(quantity, fromStack, toStack) =>
        (Integer.parseInt(quantity), 
          Integer.parseInt(fromStack), 
          Integer.parseInt(toStack))
      case _ =>
        throw new RuntimeException("Contradiction.")

  def cleanValue(string: String): String =
    import scala.util.matching.Regex
    val crate = """\[([A-Z])\](\s*)""".r
    string match
      case crate(symbol, _) => symbol
      case _ => "_"

  def cleanCrateText(text: String) =
    text
      .split("\n")
      .map(_
        .grouped(4)
        .toList)
      .init
      .toList
      .map(_.map(Crates.cleanValue(_)))
      .transpose
      .map(_.dropWhile(_ == "_"))

@main def main(arg: String): Unit =
  import scala.io.Source

  val data = Source
    .fromFile(arg)
    .getLines
    .toList
    .mkString("\n")
    .split("\n\n")

  val crateText = data(0)
  val instructionText = data(1).split("\n")
  val instructions = instructionText.map(Crates.parseInstruction(_))
  val stacks = Crates.cleanCrateText(crateText)
  val part1Crates = Crates(stacks)
  val part2Crates = Crates(stacks)

  for
    instruction <- instructions
  do
    val (qty, from, to) = instruction
    part1Crates.moveCrates(qty, from, to)
    part2Crates.moveCratesTogether(qty, from, to)

  println(part1Crates.crateStacks.toList.map(_.head).mkString)
  println(part2Crates.crateStacks.toList.map(_.head).mkString)
