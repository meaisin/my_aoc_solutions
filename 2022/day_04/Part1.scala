@main def main(arg: String): Unit =
  import scala.io.Source
  val data = Source
    .fromFile(arg)
    .getLines
    .toList
    .map(_
      .split(",")
      .toList
      .map(_
        .split("-").toList))
    .map(_
      .map{x =>
        (Integer.parseInt(x(0)), Integer.parseInt(x(1)))
      })
    .map{x =>
      (x(0), x(1))
    }
    .map(x => ((x._1._1 to x._1._2).toSet, (x._2._1 to x._2._2).toSet))

  val part1Solution = data
    .filter(x => (x._1 subsetOf x._2) || (x._2 subsetOf x._1))
    .length

  println(part1Solution)

  val part2Solution = data
    .filter(x => (x._1 intersect x._2).size > 0)
    .length

  println(part2Solution)
