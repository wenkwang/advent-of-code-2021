import scala.io.Source

object Day20ImageEnhancement {

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("image-enhancement-input.txt")
    val input = source.getLines().toList.filter(_.trim.nonEmpty)
    val code = input.head.toVector
    var image = buildImage(input.tail)
    image = expandImage(image)
    for (_ <- 1 to 50) {
      image = enhanceImage(code, image)
    }
//    val litCnt = step2.foldLeft(0)((acc, row) => row.foldLeft(acc)((a, c) => if (c == '#') a + 1 else a))
    val litCnt = countLit(image)
//    printVector(image)
    println(s"# of lit: $litCnt, m: ${image.length}, n: ${image(0).length}")
  }

  private def countLit(image: Vector[Vector[Char]]): Int = {
    var cnt = 0
    for {
      x <- 50 to image.length - 50
      y <- 50 to image(x).length - 50
    } if (image(x)(y) == '#') cnt += 1
    cnt
  }

  private def printVector(v: Vector[Vector[Char]]): Unit = println(s"${v.map(_.mkString("")).mkString("\n")}")

  private def buildImage(data: List[String]): Vector[Vector[Char]] = data.map(_.toVector).toVector

  private def enhanceImage(code: Vector[Char], image: Vector[Vector[Char]]): Vector[Vector[Char]] = {
    val expanded = image
    val m = expanded.length
    val n = expanded(0).length
    val enhanced = Array.fill(m)(Array.fill(n)('.'))
    for {
      x <- expanded.indices
      y <- expanded(x).indices
    } {
      val index = getIndex(x, y, expanded)
      enhanced(x)(y) = code(index)
    }
    enhanced.map(_.toVector).toVector
  }

  private def expandImage(image: Vector[Vector[Char]]): Vector[Vector[Char]] = {
    val m = image.length
    val n = image(0).length
    val expanded = Array.fill(m + 400)(Array.fill(n + 400)('.'))
    for {
      x <- image.indices
      y <- image(x).indices
    } expanded(x + 200)(y + 200) = image(x)(y)
    expanded.map(_.toVector).toVector
  }

  private def getIndex(x: Int, y: Int, image: Vector[Vector[Char]]): Int = {
    val delta = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 0), (0, 1), (1, -1), (1, 0), (1, 1))
    val code = delta.map { d =>
      val nx = d._1 + x
      val ny = d._2 + y
      if (nx <  0 || nx >= image.length || ny < 0 || ny >= image(nx).length) 0
      else { if (image(nx)(ny) == '#') 1 else 0 }
    }
    binaryToDecimal(code)
  }

  private def binaryToDecimal(code: List[Int]): Int = code.foldLeft(0)((acc, b) => acc * 2 + b)

}
