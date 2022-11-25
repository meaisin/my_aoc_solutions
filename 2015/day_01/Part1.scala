@main def main(arg: String): Unit =
  import scala.io.Source

  val data = Source
    .fromFile(arg)
    .getLines
    .toList
    .flatMap(_.toList.map(_.toString))
    .map{x =>
      x match
        case "(" => 1
        case ")" => -1
    }.sum

  println(data)
