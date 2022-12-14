@main def main(arg: String): Unit = {
  import scala.io.Source

  val input = Source
    .fromFile(arg)
    .getLines
    .toList
    .grouped(3)
    .map(_.take(2))
    .toList
}
