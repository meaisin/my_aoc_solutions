// Verified [but inefficient :(] solution :)

@main def main(arg: String): Unit =
  import scala.io.Source
  val file = Source
    .fromFile(arg)
    .getLines
    .toList
    .map(_.split(","))
    .flatten
    .map(Integer.parseInt(_))

  val data1 = iterate(80, file)
  println(data1.length)

  val data2 = iterate(256, file)
  println(data2.length)

def iterate(counter: Int, inputList: List[Int]): List[Int] =
  def inner(counter: Int, list: List[Int]): List[Int] =
    println(s"Counter: $counter")
    if counter == 0 then 
      list
    else
      inner(
        counter - 1,
        list.foldRight(List(): List[Int]){
          (x, acc) => if x == 0 then (8 :: 6 :: acc) else (x - 1 :: acc)
        })
  inner(counter, inputList)
