object Constants:
  def indexOfBasement(list: List[Int]): Int =
    def inner(list: List[Int], position: Int, index: Int): Int =
      if position == -1 then
        index
      else
        inner(list.tail, position + list.head, index + 1)
    inner(list, 0, 0)

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
    }

  val index = Constants.indexOfBasement(data)

  println(index)
