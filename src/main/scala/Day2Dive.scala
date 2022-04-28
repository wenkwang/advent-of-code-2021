import scala.io.Source

object Day2Dive {

  case class Move(direction: String, distance: Int)

  def main(args: Array[String]): Unit = {
    val fileName = "dive-input.txt"
    val source = Source.fromResource(fileName)
    val moves = source.getLines().map { line =>
      val res = line.split(" ").toList
      Move(res.head, res.tail.head.toInt)
    }.toList
    val (hDist, vDist) = getPosition(moves)
    val (nHDist, nVDist) = getNewPosition(moves)

    source.close()
    println(s"position is at: ($hDist, $vDist), result is ${hDist * vDist}.")
    println(s"new position is at ($nHDist, $nVDist), result is ${nHDist * nVDist}")
  }

  private def getPosition(moves: List[Move]): (Int, Int) = {
    moves.foldLeft((0, 0)) { case (acc, m) =>
      m.direction.toLowerCase match {
        case "forward" => (acc._1 + m.distance, acc._2)
        case "down" => (acc._1, acc._2 + m.distance)
        case "up" => (acc._1, acc._2 - m.distance)
      }
    }
  }

  private def getNewPosition(moves: List[Move]): (Int, Int) = {
    val (h, v, _) = moves.foldLeft((0, 0, 0)) { case (acc, m) =>
      m.direction.toLowerCase match {
        case "forward" => (acc._1 + m.distance, acc._2 + acc._3 * m.distance, acc._3)
        case "down" => (acc._1, acc._2, acc._3 + m.distance)
        case "up" => (acc._1, acc._2, acc._3 - m.distance)
      }
    }
    (h, v)
  }

}
