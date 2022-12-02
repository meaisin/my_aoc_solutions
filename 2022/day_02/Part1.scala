// A, X = Rock
// B, Y = Paper
// C, Z = Scissors

def roundScore(list: List[String]): Int =
  list match
    case List("A", "Y") => 2 + 6
    case List("B", "Y") => 2 + 3
    case List("C", "Y") => 2 + 0
    case List("A", "X") => 1 + 3 
    case List("B", "X") => 1 + 0
    case List("C", "X") => 1 + 6
    case List("A", "Z") => 3 + 0
    case List("B", "Z") => 3 + 6
    case List("C", "Z") => 3 + 3
    case _ => throw new RuntimeException("Invalid format.")

@main def main(arg: String): Unit =
  import scala.io.Source
  val data = Source
    .fromFile(arg)
    .getLines
    .toList
    .map(_
      .split(" ")
      .toList)
    .map(roundScore(_))

  println(data.sum)
