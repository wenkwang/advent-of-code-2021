import scala.collection.mutable
import scala.io.Source

object Day9SmokeBasin {

  case class Pos(x: Int, y: Int, risk: Int = 0)

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("smoke-basin-input.txt")
    val input: Array[Array[Char]] = source.getLines().map(line => line.toArray).toArray
    source.close()
//    val lowPositions: List[Pos] = findLowPositions(input)
    val input2 = input.map(row => row.map(c => if (c == '9') 'x' else c))
//    println(s"before: \n${input2.map(_.mkString(",")).mkString("\n")}")
    val basins = findBasins(input2)
//    println(s"after: \n${input2.map(_.mkString(",")).mkString("\n")}")
    println(s"basins: $basins")

//    println(s"lowPositions: $lowPositions")
//    println(s"sum of low positions risk value: ${lowPositions.map(_.risk).sum}")
    val largest3Basins = basins.toList.sortWith(_._2 > _._2).take(3)
    println(s"largest 3 basins are: $largest3Basins, multiply result is: ${largest3Basins.foldLeft(1)((acc, s) => acc * s._2)}")
  }

  private def findLowPositions(input: Array[Array[Char]]): List[Pos] = {
    val res = for {
      x <- input.indices
      y <- input(x).indices
      if isLowPosition(x, y, input)
    } yield Pos(x, y, (input(x)(y) - '0') + 1)
    res.toList
  }

  private def isLowPosition(x: Int, y: Int, input: Array[Array[Char]]): Boolean = {
    val delta = Vector((-1, 0), (0, -1), (1, 0), (0, 1))
    val res: Vector[Boolean] = for {
      d <- delta
      dx = x + d._1
      dy = y + d._2
      if dx >= 0 && dx < input.length && dy >= 0 && dy < input(x).length
    } yield input(x)(y) < input(dx)(dy)
    res.forall(_ == true)
  }

  private def findBasins(input: Array[Array[Char]]): Map[Pos, Int] = {
    val posToSize = for {
      x <- input.indices
      y <- input(x).indices
      if input(x)(y) != 'x' && input(x)(y) != 'o' // x is barrier, o is visited point
    } yield bfs(x, y, input)
    posToSize.toMap
  }

  private def bfs(x: Int, y: Int, input: Array[Array[Char]]): (Pos, Int) = {
    val pos = Pos(x, y)
    var size = 0
    input(x)(y) = 'o'
    val queue = mutable.Queue(pos)
    while (queue.nonEmpty) {
      val p = queue.dequeue()
      size += 1
      val neighbors = getNeighbors(p, input)
      neighbors.foreach(p => input(p.x)(p.y) = 'o')
      queue.appendAll(neighbors)
    }
    (pos, size)
  }

  private def getNeighbors(pos: Pos, input: Array[Array[Char]]): Seq[Pos] = {
    val delta = Vector((-1, 0), (0, -1), (1, 0), (0, 1))
    for {
      d <- delta
      dx = pos.x + d._1
      dy = pos.y + d._2
      if dx >= 0 && dx < input.length && dy >= 0 && dy < input(dx).length
      if input(dx)(dy) != 'o' && input(dx)(dy) != 'x'
    } yield Pos(dx, dy)
  }

}