import scala.io.Source

object Day5HydrothermalVenture {

  private var posToCount = Map.empty[Pos, Int]

  def main(args: Array[String]): Unit = {
    val fileName = "hydro-vent-input.txt"
    val source = Source.fromResource(fileName)
    val rawLines = source.getLines().toList
    source.close()

    val lines = rawLines.map(buildLine)
    val part1Lines = lines.filter(line => line.isHorizontal || line.isVertical)
    val part2Lines = lines.filter(line => line.isDiagonal)
    part1Lines.foreach(markLine)
    part2Lines.foreach(markDiagonalLine)
    val overlappedPoints = posToCount.filter(_._2 >= 2)
    println(s"# of overlapped points: ${overlappedPoints.size}")
  }

  private def markLine(line: Line): Unit = {
    for {
      x <- line.start.x to line.end.x
      y <- line.start.y to line.end.y
    } yield {
      val count = posToCount.getOrElse(Pos(x, y), 0)
      posToCount += (Pos(x, y) -> (count + 1))
    }
  }

  private def markDiagonalLine(line: Line): Unit = {
    val dx = if (line.start.x < line.end.x) 1 else -1
    val dy = if (line.start.y < line.end.y) 1 else -1
    val range = Math.abs(line.start.x - line.end.x)
    posToCount += (line.start -> (posToCount.getOrElse(line.start, 0) + 1))
    var x = line.start.x
    var y = line.start.y
    for {
      _ <- 1 to range
    } yield {
      x += dx
      y += dy
      val p = Pos(x, y)
      val count = posToCount.getOrElse(p, 0)
      posToCount += (p -> (count + 1))
    }
  }

  private def buildLine(raw: String): Line = {
    val pos = raw.split(" -> ")
    val start = pos(0).split(",").map(_.toInt)
    val end = pos(1).split(",").map(_.toInt)
    (start, end) match {
      case (s, e) if s(0) < e(0) || s(1) < e(1) => Line(Pos(start(0), start(1)), Pos(end(0), end(1)))
      case _ => Line(Pos(end(0), end(1)), Pos(start(0), start(1)))
    }
  }

}

case class aPos(x: Int, y: Int)

case class Line(start: Pos, end: Pos) {
  val isHorizontal: Boolean = start.y == end.y

  val isVertical: Boolean = start.x == end.x

  val isDiagonal: Boolean = Math.abs(start.x - end.x) == Math.abs(start.y - end.y)
}
