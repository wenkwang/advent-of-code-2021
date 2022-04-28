import scala.io.Source

object Day8SegmentDisplay {

  val segmentsToNumber = Map(
    "012456" -> 0,
    "25" -> 1,
    "02346" -> 2,
    "02356" -> 3,
    "1235" -> 4,
    "01356" -> 5,
    "013456" -> 6,
    "025" -> 7,
    "0123456" -> 8,
    "012356" -> 9
  )

  val easyDigitsLengths = Set(2, 3, 4, 7)
  val allSegments = "abcdefg"

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("segment-display-input.txt")
    val inputList = source.getLines().toList
    source.close()
    val patternAndOutput: List[(List[String], List[String])] = inputList.map { case line =>
      val parts = line.split(" \\| ")
      (parts(0).split(" ").toList, parts(1).split(" ").toList)
    }
    val numOfEasyDigits = patternAndOutput.map(entry => entry._2.count(a => easyDigitsLengths.contains(a.length))).sum

    val decodedOutput = patternAndOutput.map(po => decode(po._1, po._2))
//    println(s"decodedOutput: $decodedOutput")
    println(s"# of easy digits is: $numOfEasyDigits")
    println(s"sum of decoded output values: ${decodedOutput.sum}")
  }

  private def decode(patterns: List[String], output: List[String]): Int = {
    val decodeMap = decodeSegmentsMapping(patterns)
//    println(s"decodeMap: $decodeMap")
    output.foldLeft(0) { case (acc, entry) =>
      val digit = decodeEntry(decodeMap, entry)
      acc * 10 + digit
    }
  }

  private def decodeEntry(decodeMap: Map[Char, Int], entry: String): Int = {
    val decoded = entry.map(c => decodeMap(c)).sorted.mkString("")
    segmentsToNumber(decoded)
  }

  private def decodeSegmentsMapping(patterns: List[String]): Map[Char, Int] = {
    var numToChars = Map(
      0 -> Set.empty[Char],
      1 -> Set.empty[Char],
      2 -> Set.empty[Char],
      3 -> Set.empty[Char],
      4 -> Set.empty[Char],
      5 -> Set.empty[Char],
      6 -> Set.empty[Char]
    )
    numToChars = processEasyDigits(patterns.filter(p => easyDigitsLengths.contains(p.length)), numToChars)
    numToChars = processCommonSegmentsFor023569(patterns.filterNot(p => easyDigitsLengths.contains(p.length)), numToChars)
    numToChars = processCommonSegmentsFor235(patterns.filter(p => p.length == 5), numToChars)
    numToChars = processCommonSegmentsFor069(patterns.filter(p => p.length == 6), numToChars)
    numToChars.map { case (num, chs) => chs.head -> num }
  }

  private def processEasyDigits(patterns: List[String], numToChars: Map[Int, Set[Char]]): Map[Int, Set[Char]] = {
    val d1 = patterns.find(_.length == 2).get
    val d4 = patterns.find(_.length == 4).get
    val d7 = patterns.find(_.length == 3).get
    val d8 = patterns.find(_.length == 7).get
    numToChars ++ Map(
      0 -> d7.diff(d1).toSet,
      2 -> d4.intersect(d7).toSet,
      5 -> d4.intersect(d7).toSet,
      1 -> d4.diff(d1).toSet,
      3 -> d4.diff(d1).toSet,
      4 -> d8.diff(d7).diff(d4).diff(d1).toSet,
      6 -> d8.diff(d7).diff(d4).diff(d1).toSet,
    )
  }

  private def processCommonSegmentsFor023569(patterns: List[String], numToChars: Map[Int, Set[Char]]): Map[Int, Set[Char]] = {
    val commonChars = patterns.foldLeft(allSegments)((acc, p) => acc.intersect(p)).toList
    val for6 = commonChars.filterNot(c => numToChars(0).contains(c)).head
    val for4 = numToChars(6).filterNot(_ == for6).head
    numToChars ++ Map(
      4 -> Set(for4),
      6 -> Set(for6)
    )
  }

  private def processCommonSegmentsFor235(patterns: List[String], numToChars: Map[Int, Set[Char]]): Map[Int, Set[Char]] = {
    val commonChars = patterns.foldLeft(allSegments)((acc, p) => acc.intersect(p)).toList
    val for3 = commonChars.filterNot(c => numToChars(0).contains(c) || numToChars(6).contains(c)).head
    val for1 = numToChars(3).filterNot(_ == for3).head
    numToChars ++ Map(
      1 -> Set(for1),
      3 -> Set(for3)
    )
  }

  private def processCommonSegmentsFor069(patterns: List[String], numToChars: Map[Int, Set[Char]]): Map[Int, Set[Char]] = {
    val commonChars = patterns.foldLeft(allSegments)((acc, p) => acc.intersect(p)).toList
    val for5 = commonChars.filterNot(c => numToChars(0).contains(c) || numToChars(1).contains(c) || numToChars(6).contains(c)).head
    val for2 = numToChars(5).filterNot(_ == for5).head
    numToChars ++ Map(
      2 -> Set(for2),
      5 -> Set(for5)
    )
  }

}
