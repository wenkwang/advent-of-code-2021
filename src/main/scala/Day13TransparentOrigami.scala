import scala.io.Source

object Day13TransparentOrigami {

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("transparent-origami-input.txt")
    val (rawCmds, rawPoints) = source.getLines().toList.filterNot(_.trim.isEmpty).partition(_.startsWith("fold along"))
    source.close()
    val instructions: Vector[(String, Int)] = rawCmds.map(_.drop(11)).map { case i =>
      val pair = i.split("=")
      (pair(0), pair(1).toInt)
    }.toVector
    val points = rawPoints.map { case p =>
      val pair = p.split(",")
      (pair(0).toInt, pair(1).toInt)
    }

    val foldedPoints = foldPaper(points, instructions).distinct
    println(s"foldedPoints: $foldedPoints")
    println(s"# of points after 1st fold: ${foldedPoints.size}")

    plotPoints(foldedPoints)
  }

  private def foldPaper(points: List[(Int, Int)], instructions: Vector[(String, Int)], step: Int = -1): List[(Int, Int)] = {
    var pointsToUpdate = points
    val totalRuns = if (step == -1) instructions.size else step
    for (x <- 0 until totalRuns) {
      println(s"processing w/ instruction: ${instructions(x)}")
      val isX = instructions(x)._1 == "x"
      val foldLine = instructions(x)._2
      pointsToUpdate = pointsToUpdate.map { case p =>
        if (isX) {
          if (p._1 < foldLine) p
          else (2 * foldLine - p._1, p._2)
        } else {
          if (p._2 < foldLine) p
          else (p._1, 2 * foldLine - p._2)
        }
      }
    }
    pointsToUpdate.toList
  }

  private def plotPoints(points: List[(Int, Int)]): Unit = {
    println(s"${points.sortBy(p => (p._2, p._1))}")
    val sb = new StringBuilder

    def plot(points: List[(Int, Int)], curPos: (Int, Int)): Unit = points match {
      case Nil => sb
      case head :: tail => {
        if (curPos._2 == head._2) {
          for (x <- curPos._1 + 1 until head._1)(sb.append("-"))
        } else {
          for (y <- curPos._2 until head._2)(sb.append("\n"))
          for (x <- 0 until head._1)(sb.append("-"))
        }
        sb.append("#")
        plot(tail, (head._1, head._2))
      }
    }

    plot(points.sortBy(p => (p._2, p._1)), (0, -1))
    println(sb.toString())
  }

}
