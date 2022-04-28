import scala.collection.mutable
import scala.io.Source

object Day11FlashOctopus {

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("flash-octopus-input.txt")
    val grid: Array[Array[Int]] = source.getLines().toList.map(line => line.toArray.map(_ - '0')).toArray
    source.close()

//    var flash = 0
//    printArray(grid)
    var step = 0
    var count = 0
    while (count < 100) {
      step += 1
      count = oneStep(grid)
    }

    println(s"first time when all flash: $step")

  }

  private def oneStep(grid: Array[Array[Int]]): Int = {
    val starters = for {
      x <- grid.indices
      y <- grid(x).indices
    } yield {
      grid(x)(y) += 1
      if (grid(x)(y) > 9) Some((x, y))
      else None
    }
    bfs(starters.flatten.toList, grid)
  }

  private def bfs(points: List[(Int, Int)], grid: Array[Array[Int]]): Int = {
    val delta = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
    val flashed = mutable.Set.empty[(Int, Int)]
    val queue = mutable.Queue.empty[(Int, Int)]
    queue.enqueueAll(points)
    while (queue.nonEmpty) {
      val p = queue.dequeue()
      flashed.add(p)
      for {
        d <- delta
        nx = p._1 + d._1
        ny = p._2 + d._2
        if (nx >= 0 && nx < grid.length && ny >= 0 && ny < grid(nx).length)
        if (grid(nx)(ny) < 10)
      } {
        grid(nx)(ny) += 1
        if (grid(nx)(ny) > 9) queue.enqueue((nx, ny))
      }
    }
    flashed.foreach(fp => grid(fp._1)(fp._2) = 0)
    flashed.size
  }

  private def printArray(a: Array[Array[Int]]): Unit =
    println(s"${a.map(row => row.toList.mkString(",")).toList.mkString("\n")}\n\n")

}
