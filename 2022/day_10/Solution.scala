class CPUState(val cycle: Int, val signal: Int):
  override def toString: String = s"[$cycle]: $signal"

  def draw: String =
    val position = (cycle.-(1) % 40)
    if signalWidth contains position then "#" else "."

  val signalStrength: Int = cycle * signal

  def nextCycle: CPUState =
    CPUState(cycle + 1, signal)

  def changeSignal(by: Int): CPUState =
    CPUState(cycle, signal + by)

  def signalWidth: List[Int] =
    List(signal - 1, signal, signal + 1)

end CPUState

class Device:
  var cpuStates = Vector(CPUState(1, 1))

  def lastState: CPUState = cpuStates.last

  def handleInstruction(instr: String): Unit =
    instr.split(" ").toList match
      case List("noop") => cpuStates = cpuStates appended lastState.nextCycle
      case List("addx", n) =>
        cpuStates = cpuStates appended lastState.nextCycle
        cpuStates = cpuStates appended lastState.nextCycle.changeSignal(n.toInt)
      case _ => throw new RuntimeException("Unsupported instruction.")

end Device

@main def main(arg: String): Unit =
  import scala.io.Source

  val data = Source
    .fromFile(arg)
    .getLines

  val device: Device = Device()

  data.foreach{x =>
    device.handleInstruction(x)
  }

  val part1Solution = List(20, 60, 100, 140, 180, 220)
    .map(_ - 1)
    .map(device.cpuStates(_).signalStrength)
    .sum

  println(part1Solution)

  val part2Solution = device
    .cpuStates
    .grouped(40)
    .map(_
      .map(_.draw).mkString)
    .mkString("\n")

  println(part2Solution)
