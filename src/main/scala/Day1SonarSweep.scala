import scala.annotation.tailrec
import scala.io.Source

object Day1SonarSweep {

  def main(args: Array[String]): Unit = {
    val fileName = "sonar-sweep-input.txt"
    val numbers = getInputNumbers(fileName)
    val increaseRes = if (numbers.isEmpty) 0 else countIncrease(numbers, 0)
    val increase3SumRes = if (numbers.size <= 3) 0 else count3SumIncrease(numbers.take(3), numbers.drop(3), 0)
    println(s"# of increases: $increaseRes")
    println(s"# of 3 sum increases: $increase3SumRes")
  }

  def getInputNumbers(file: String): List[Int] = {
    val source = Source.fromResource(file)
    val lines = source.getLines().toList.map(_.toInt)
    source.close()
    lines
  }

  @tailrec
  def countIncrease(numbers: List[Int], acc: Int): Int = numbers match {
    case _ :: Nil => acc
    case elem :: tail => if (elem < tail.head) countIncrease(tail, acc + 1) else countIncrease(tail, acc)
  }


  def count3SumIncrease(window: List[Int], numbers: List[Int], acc: Int): Int = numbers match {
    case Nil => acc
    case e :: tail =>
      if (e > window.head) count3SumIncrease(window.tail :+ e, tail, acc + 1) else count3SumIncrease(window.tail :+ e, tail, acc)
  }

}
