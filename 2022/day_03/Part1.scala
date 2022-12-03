def valueOf(char: Char): Int =
  if char.isUpper then (char.toInt - 38) else (char.toInt - 96)

@main def main(arg: String): Unit =
  import scala.io.Source
  val data = Source
    .fromFile(arg)
    .getLines
    .toList
    .map{x => 
      (x.take(x.length / 2).toSet, x.drop(x.length / 2).toSet)
    }
    .map{x =>
      x._1 intersect x._2
    }
    .map(_.toList)
    .flatten
    .map{x =>
      if x.isUpper then (x.toInt - 38) else (x.toInt - 96)
    }
    .sum

  println(data)
