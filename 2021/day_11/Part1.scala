// Firstly, I need to way to model a single step.
// A single step consists of:
// - Incrementing all energy levels by 1.
// - Each octopus > 9 flashes, raising the energy of all adjacent by 1.
// - This continues until no more actions take place.
// - All octopus > 9 is set back to 0.

// (-1, -1) (-1, +0) (-1, +1)
// (+0, -1) (+0, +0) (+0, +1)
// (+1, -1) (+1, +0) (+1, +1)

class Octopus(octopuses: List[List[Int]]):
  require(octopuses.length == 10 && octopuses.head.length == 10)
  private val mapLimit = 9

  override def toString: String =
    octopuses.map(_.mkString).mkString("\n")

  private def initiateStep: Octopus =
    Octopus(octopuses
      .map(_.map(_ + 1)))

  private def endStep: Octopus =
    Octopus(octopuses
      .map(_.map(x => if x > 9 then 0 else x)))

  def step: Octopus =
    this
      .initiateStep
      .endStep

@main def main(arg: String): Unit =
  import scala.io.Source

  val data = Source
    .fromFile(arg)
    .getLines
    .toList
    .map(_.toList.map(x => Integer.parseInt(x.toString)).toList)

  val octopuses = Octopus(data)

  println(octopuses)
  println("")
  println(octopuses.step)
  println("")
  println(octopuses.step.step)
