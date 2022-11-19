class Display(samples: List[String], outputValue: List[String]):
  val sampleSet = samples.map(_.toSet)
  val oneValue = sampleSet.filter(_.size == 2).head
  val fourValue = sampleSet.filter(_.size == 4).head
  val sevenValue = sampleSet.filter(_.size == 3).head
  val eightValue = sampleSet.filter(_.size == 7).head

  val twoThreeFive = sampleSet.filter(_.size == 5)
  val nineSixZero = sampleSet.filter(_.size == 6)

  //   aaaa
  //  b    c
  //  b    c
  //   dddd
  //  e    f
  //  e    f
  //   gggg
  //
  //  a = topSegment :)
  //  b = topLeftSegment :)
  //  c = topRightSegment :)
  //  d = middleSegment :)
  //  e = bottomLeftSegment :)
  //  f = bottomRightSegment :)
  //  g = bottomSegment :)
  //
  //   aaaa 
  //  b    c
  //  b    c
  //   dddd
  //  e    ?
  //  e    ?
  //   ????
  //
  // 1 = cf
  // 7 = acf
  // Tf 7 - 1 = a
  
  val ecd = nineSixZero.map(eightValue diff _).flatten.toSet

  // a
  val top = sevenValue diff oneValue // :)
  
  // e
  val bottomLeft = ecd diff fourValue // :)

  val cd = ecd diff bottomLeft

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

  val oneValueCheck = oneValue intersect (topRight union bottomRight)
  val fourValueCheck = fourValue intersect (topLeft union middle union topRight union bottomRight)

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

  data.foreach{ x =>
    println(s"Display: \n$x")
    println(s"-- [1]: ${x.oneValue}")
    println(s"-- [4]: ${x.fourValue}")
    println(s"-- [7]: ${x.sevenValue}")
    println(s"-- [8]: ${x.eightValue}")
    println(s"Decoding:\n")
    println(s"-- [0]: ${x.decodedZero}")
    println(s"-- [1]: ${x.decodedOne}")
    println(s"-- [2]: ${x.decodedTwo}")
    println(s"-- [3]: ${x.decodedThree}")
    println(s"-- [4]: ${x.decodedFour}")
    println(s"-- [5]: ${x.decodedFive}")
    println(s"-- [6]: ${x.decodedSix}")
    println(s"-- [7]: ${x.decodedSeven}")
    println(s"-- [8]: ${x.decodedEight}")
    println(s"-- [9]: ${x.decodedNine}")
  }
