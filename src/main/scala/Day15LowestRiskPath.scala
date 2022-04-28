import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day15LowestRiskPath {

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("lowest-risk-path-input.txt")
    val matrix: Array[Array[Int]] = source.getLines().toArray.map(row => row.toArray.map(c => c - '0'))
    val expandedMatrix = expandMatrix(matrix)
//    println(s"matrix: ${expandedMatrix.map(_.toList).toList.mkString("\n")}")
    val ps = new PathSearch(expandedMatrix)
    val risk = ps.findLowestRisk(Point(0, 0), Point(expandedMatrix.length - 1, expandedMatrix(0).length - 1))
    println(s"lowest risk is: $risk")
  }

  private def expandMatrix(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    val n = matrix.length
    val m = matrix(0).length
    val expanded = Array.fill(n * 5)(Array.fill(m * 5)(0))
    for (lvl <- 0 to 4) {
      for {
        x <- matrix.indices
        y <- matrix(x).indices
      } {
        val rx = lvl * n + x
        val value = matrix(x)(y) + lvl
        expanded(rx)(y) = if (value > 9) value - 9 else value
      }
    }
    for (lvl <- 1 to 4) {
      for {
        x <- expanded.indices
        y <- matrix(0).indices
      } {
        val ry = lvl * n + y
        val value = expanded(x)(y) + lvl
        expanded(x)(ry) = if (value > 9) value - 9 else value
      }
    }
    expanded
  }
}

case class Point(x: Int, y: Int)

class PathSearch(matrix: Array[Array[Int]]) {

  private val toVisitPoints: mutable.Map[Point, Int] = mutable.Map.empty[Point, Int]
  private val visitedPoints: mutable.Map[Point, Int] = mutable.Map.empty[Point, Int]

  def findLowestRisk(start: Point, end: Point): Int = {
    toVisitPoints.put(start, 0)
    var risk = 0
    while (!visitedPoints.keySet.contains(end)) {
//      println(s"visitedPoints: $visitedPoints")
//      println(s"toVisitPoints: $toVisitPoints")
      val nextPoints = findNextPoints()
      risk = toVisitPoints(nextPoints.head)
      nextPoints.foreach { np =>
        visitedPoints.put(np, risk)
        toVisitPoints.remove(np)
      }
      updateMapping(nextPoints)
    }
    risk
  }

  private def findNextPoints(): List[Point] = {
    val points = toVisitPoints.toList.sortBy(_._2)
    points.filter(e => e._2 == points.head._2).map(_._1)
  }

  private def updateMapping(points: List[Point]): Unit = {
    val delta = Vector((-1, 0), (0, -1), (0, 1), (1, 0))
    points.foreach { p =>
      for (x <- delta.indices) {
        val nx = p.x + delta(x)._1
        val ny = p.y + delta(x)._2
        if (nx >= 0 && nx < matrix.length && ny >= 0 && ny < matrix(nx).length) {
          val np = Point(nx, ny)
          if (!visitedPoints.keySet.contains(np)) {
            val dist = visitedPoints(p) + matrix(nx)(ny)
            if (!toVisitPoints.contains(np) || toVisitPoints.contains(np) && dist < toVisitPoints(np)) toVisitPoints.put(np, dist)
          }
        }
      }
    }
  }

}
