import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day3BinaryDiagnostic {

  def main(args: Array[String]): Unit = {
    val fileName = "binary-diagnostic-input.txt"
    val source = Source.fromResource(fileName)
    val lines = source.getLines().toList
    source.close()

    val gammaCode = decode(lines)
    val epsilonCode = convert(gammaCode)
    val gamma = Integer.parseInt(gammaCode, 2)
    val epsilon = Integer.parseInt(epsilonCode, 2)

    val oxygenCode = filterNumbers(lines, 0, findMostCommonChar)
    val co2Code = filterNumbers(lines, 0, findLeastCommonChar)
    val oxygenRate = Integer.parseInt(oxygenCode, 2)
    val co2Rate = Integer.parseInt(co2Code, 2)

    println(s"gamma: $gamma, epsilon: $epsilon, power consumption is: ${gamma * epsilon}")
    println(s"oxygen: $oxygenRate, co2: $co2Rate, life support rate is: ${oxygenRate * co2Rate}")
  }

  private def decode(lines: List[String]): String = {
    val res = Array.fill(lines.head.length){0}
    val size = lines.size
    lines.map { line =>
      line.zipWithIndex.map{ case (c, i) => res(i) = res(i) + (c - '0') }
    }
    res.toList.map(acc => if (acc > size / 2) 1 else 0).mkString("")
  }

  private def convert(code: String): String = {
    code.map(c => if (c == '0') '1' else '0')
  }

  @tailrec
  private def filterNumbers(numbers: List[String], index: Int, f: (List[String], Int) => Char): String = numbers match {
    case elem :: Nil => elem
    case elem :: tail =>
      val mcChar = f(numbers, index)
      filterNumbers(numbers.filter(num => num.charAt(index) == mcChar), index + 1, f)
  }

  private def findMostCommonChar(numbers: List[String], index: Int): Char = {
    val res = numbers.foldLeft(0)((acc, num) => acc + num.charAt(index) - '0')
    if (res >= (numbers.size + 1) / 2) '1' else '0'
  }

  private def findLeastCommonChar(numbers: List[String], index: Int): Char = {
    val res = numbers.foldLeft(0)((acc, num) => acc + num.charAt(index) - '0')
    if (res >= (numbers.size + 1) / 2) '0' else '1'
  }

}
