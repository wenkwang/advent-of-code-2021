import scala.io.Source

object Day6LanternFish {

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("lantern-fish-input.txt")
    val fishArray: Array[Long] = Array.fill(9)(0L)
    val n = 256
    val nums = source.getLines().toList.head.split(",").map(_.toInt)
    nums.foreach(n =>
      fishArray(n) += 1
    )
    source.close()
    val fs = FishSchool(fishArray)
    fs.plusNDays(n)
    println(s"# of fish after $n days: ${fs.getSchoolSize}")
  }

}

case class FishSchool(fish: Array[Long]) {

  private val fishSchool: Array[Long] = fish

  def plusNDays(n: Int): Unit = for {
    _ <- 1 to n
  } yield plusOneDay

  def plusOneDay: Unit = {
    val f0 = fishSchool(0)
    for {
      x <- 0 to 7
    } yield (fishSchool(x) = fishSchool(x + 1))
    fishSchool(6) += f0
    fishSchool(8) = f0
//    printFishSchool
  }

  def getSchoolSize: Long = fishSchool.sum

  private def printFishSchool = println(s"fish school: ${fishSchool.zipWithIndex.mkString(";")}")

}
