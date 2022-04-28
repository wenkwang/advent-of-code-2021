import scala.io.Source

object Day18SnailFish {

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("snail-fish-input.txt")
    val numbers = source.getLines().toList.map(line => RegNum.buildMagicNumber(line))
    source.close()
    println(s"pairs loaded: \n${numbers.mkString("\n")}")
    val reducedNumber = numbers.tail.foldLeft(numbers.head)((n1, n2) => Calculator.add(n1, n2))
    println(s"reduced result: $reducedNumber")
    val magnitude = Calculator.getMagnitude(reducedNumber)
    println(s"magnitude is: $magnitude")
    val source2 = Source.fromResource("snail-fish-input.txt")
    val numbers2 = source2.getLines().toList.map(line => RegNum.buildMagicNumber(line))
    source2.close()
    val maxTwoSumMagnitude = Calculator.getMaxMagnitudeFromTwoAdd(numbers2.toVector)
    println(s"max two sum magnitude is: $maxTwoSumMagnitude")
  }
}

object RegNum {
  def buildMagicNumber(raw: String, depth: Int = 0): Vector[RegNum] = {
    if (raw.length == 1) Vector(RegNum(raw.toList.head - '0', depth))
    else {
      val pivot = findSplitIndex(raw)
      buildMagicNumber(raw.substring(1, pivot), depth + 1) ++ buildMagicNumber(raw.substring(pivot + 1, raw.length - 1), depth + 1)
    }
  }

  def clone(n: Vector[RegNum]): Vector[RegNum] = {
    n.foldLeft(Vector.empty[RegNum])((acc, n) => acc :+ RegNum(n.value, n.depth))
  }

  private def findSplitIndex(raw: String): Int = {
    var lcnt = 0
    var pivot = -1
    var index = 1
    while (pivot < 0) {
      if (raw.charAt(index) == '[') lcnt += 1
      else if (raw.charAt(index) == ']') lcnt -= 1
      else if (raw.charAt(index) == ',' && lcnt == 0) pivot = index
      index += 1
    }
    pivot
  }
}

case class RegNum(var value: Int, var depth: Int) {
  override def toString: String = s"${value.toString}($depth)"
}

object Calculator {
  def add(n1: Vector[RegNum], n2: Vector[RegNum]): Vector[RegNum] = {
//    println(s"n1: $n1\nn2: $n2")
    var num = mergeNumbers(n1, n2)
    var isDone = false
    while(!isDone) {
      val reduced = explode(num)
      if (reduced == num) {
        val splitted = split(reduced)
        if (splitted == reduced) isDone = true
        else num = splitted
      } else num = reduced
    }
    num
  }

  def getMagnitude(num: Vector[RegNum]): Int = {
    var numToProcess = num
    while (numToProcess.length > 1) {
//      println(s"num: $numToProcess")
      val curDepth = numToProcess.map(_.depth).max
      numToProcess = updateNumWithMagnitude(numToProcess, curDepth)
    }
    numToProcess.head.value
  }

  def getMaxMagnitudeFromTwoAdd(nums: Vector[Vector[RegNum]]): Int = {
    var max = 0
    for {
      x <- nums.indices
      y <- nums.indices
      if x != y
    } {
      val reducedNum = add(RegNum.clone(nums(x)), RegNum.clone(nums(y)))
      val magnitude = getMagnitude(reducedNum)
      max = math.max(max, magnitude)
    }
    max
  }

  private def updateNumWithMagnitude(n: Vector[RegNum], depth: Int): Vector[RegNum] = {
    val numWithIndex: Option[(RegNum, Int)] = n.zipWithIndex.find(_._1.depth == depth)
    numWithIndex.fold(n) { entry =>
      val nv = entry._1.value * 3 + n(entry._2 + 1).value * 2
      (n.take(entry._2) :+ RegNum(nv, depth - 1)) ++ n.drop(entry._2 + 2)
    }
  }

  private def mergeNumbers(n1: Vector[RegNum], n2: Vector[RegNum]): Vector[RegNum] = {
    n1.foreach(n => n.depth += 1)
    n2.foreach(n => n.depth += 1)
    n1 ++ n2
  }

  private def explode(num: Vector[RegNum]): Vector[RegNum] = {
    val numWithIndex: Option[(RegNum, Int)] = num.zipWithIndex.find(_._1.depth >= 5)
    numWithIndex.fold(num) { entry =>
      val lv = entry._1
      val rv = num(entry._2 + 1)
      explodeToLeft(num, lv, entry._2)
      explodeToRight(num, rv, entry._2 + 1)
      (num.take(entry._2) :+ RegNum(0, entry._1.depth - 1)) ++ num.drop(entry._2 + 2)
    }
  }

  private def explodeToLeft(n: Vector[RegNum], v: RegNum, k: Int): Unit =
    if (k > 0) n(k - 1).value += v.value

  private def explodeToRight(n: Vector[RegNum], v: RegNum, k: Int): Unit =
    if (k < n.length - 1) n(k + 1).value += v.value

  private def split(num: Vector[RegNum]): Vector[RegNum] = {
    math.pow(9d, 14d)
    val numWithIndex: Option[(RegNum, Int)] = num.zipWithIndex.find(_._1.value >= 10)
    numWithIndex.fold(num) { entry =>
      val rn1 = RegNum(entry._1.value / 2, entry._1.depth + 1)
      val rn2 = RegNum((entry._1.value + 1) / 2, entry._1.depth + 1)
      num.take(entry._2) ++ Vector(rn1, rn2) ++ num.drop(entry._2 + 1)
    }
  }
}