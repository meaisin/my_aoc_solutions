// Verified solution :)

class Display(samples: List[String], outputValue: List[String]):
  val sampleSet = samples.map(_.toSet)
  val outputSet = outputValue.map(_.toSet)
  val oneValue = sampleSet.filter(_.size == 2).head
  val fourValue = sampleSet.filter(_.size == 4).head
  val sevenValue = sampleSet.filter(_.size == 3).head
  val eightValue = sampleSet.filter(_.size == 7).head

  val twoThreeFive = sampleSet.filter(_.size == 5)
  val nineSixZero = sampleSet.filter(_.size == 6)

  private val ecd = nineSixZero.map(eightValue diff _).flatten.toSet

  // a
  val top = sevenValue diff oneValue // :)
  
  // e
  val bottomLeft = ecd diff fourValue // :)

  private val cd = ecd diff bottomLeft

  // d
  val middle = cd diff oneValue

  // c
  val topRight = cd diff middle

  // b
  val topLeft = fourValue diff (cd union oneValue)

  // f
  val bottomRight = oneValue diff topRight

  // g
  val bottom = eightValue diff (top union topLeft union topRight union middle union bottomLeft union bottomRight)

  val decodedZero = top union topRight union topLeft union bottomRight union bottomLeft union bottom
  val decodedOne = topRight union bottomRight
  val decodedTwo = top union topRight union middle union bottomLeft union bottom
  val decodedThree = top union topRight union middle union bottomRight union bottom
  val decodedFour = topLeft union topRight union middle union bottomRight
  val decodedFive = top union topLeft union middle union bottomRight union bottom
  val decodedSix = top union topLeft union middle union bottomLeft union bottomRight union bottom
  val decodedSeven = top union topRight union bottomRight
  val decodedEight = top union topLeft union topRight union middle union bottomRight union bottomLeft union bottom
  val decodedNine = top union topLeft union topRight union middle union bottomRight union bottom

  val decodingMap = Map(decodedZero -> "0", decodedOne -> "1", decodedTwo -> "2", decodedThree -> "3", decodedFour -> "4", decodedFive -> "5",
    decodedSix -> "6", decodedSeven -> "7", decodedEight -> "8", decodedNine -> "9")

  val decodedSampleSet = sampleSet.map(decodingMap(_))
  val decodedOutputSet = outputSet.map(decodingMap(_))

  val decodedValue = decodedOutputSet.toList.mkString

  val decodedInt = Integer.parseInt(decodedValue: String)

  override def toString(): String =
    List(s" ${top.head.toString * 4} ",
      s"${topLeft.head}    ${topRight.head}",
      s"${topLeft.head}    ${topRight.head}",
      s" ${middle.head.toString * 4} ",
      s"${bottomLeft.head}    ${bottomRight.head}",
      s"${bottomLeft.head}    ${bottomRight.head}",
      s" ${bottom.head.toString * 4} ").mkString("\n")

@main def main(arg: String): Unit =
  import scala.io.Source
  val data = Source
    .fromFile(arg)
    .getLines
    .toList
    .map(_.split(" "))
    .map(x => Display(x.takeWhile(_ != "|").toList, x.dropWhile(_ != "|").tail.toList))
    .map(_.decodedInt)
    .sum

  println(data)
