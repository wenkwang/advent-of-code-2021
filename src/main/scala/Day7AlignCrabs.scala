import scala.collection.immutable.ListMap
import scala.io.Source

object Day7AlignCrabs {

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("crab-positions-input.txt")
    val positions = source.getLines().toList.head.split(",").map(_.toInt)
    source.close()

    val posToCount = buildPosToCount(positions)
    val posToSplit = buildPosToSplit(positions.length, posToCount)
    val posToPosDist = buildPosToPosDist(positions)
//    println(s"posToPosDist: $posToPosDist")
    val (target, fuel) = getTargetFuel(posToCount, posToSplit)
    val (target1, fuel1) = getWeightedTargetFuel(posToCount, posToPosDist)

    println(s"target position is at: $target, fuel amount is: $fuel")
    println(s"weighted target position is at: $target1, fuel amount is: $fuel1")
  }

  private def buildPosToPosDist(positions: Array[Int]): Map[Int, Map[Int, Int]] = {
    (positions.min to positions.max).map { case p =>
      val distMap = buildDistMap(p, positions)
      (p -> distMap)
    }.toMap
  }

  private def buildDistMap(pos: Int, positions: Array[Int]): Map[Int, Int] = {
    positions.map { case np =>
      np -> getFuel(pos, np)
    }.toMap
  }

  private def buildPosToCount(positions: Array[Int]): ListMap[Int, Int] =
    positions.sorted.foldLeft(ListMap.empty[Int, Int]) { case (acc, p) =>
      acc + (p -> (acc.getOrElse(p, 0) + 1))
    }

  private def buildPosToSplit(size: Int, posToCount: ListMap[Int, Int]): ListMap[Int, (Int, Int)] = {
    var count = 0
    posToCount.map { case (p, c) =>
      val curCnt = count
      val restCnt = size - count
      count += c
      p -> (curCnt, restCnt)
    }
  }

  private def getTargetFuel(posToCount: ListMap[Int, Int], posToSplit: ListMap[Int, (Int, Int)]): (Int, Int) = {
    val head = posToCount.head._1
    val initFuel = posToCount.tail.map { case (p, c) => (p - head) * c }.sum
    var (target, minimumFuel) = (head, initFuel)
    posToSplit.foldLeft((head, initFuel)) { case (prev, current) =>
      val extraFuel = (current._2._1 - current._2._2) * (current._1 - prev._1)
      //      println(s"current pos: ${current._1}, extraFuel: $extraFuel")
      val fuelNeeded = prev._2 + extraFuel
      if (fuelNeeded < minimumFuel) {
        target = current._1
        minimumFuel = fuelNeeded
      }
      (current._1, fuelNeeded)
    }
    (target, minimumFuel)
  }

  private def getWeightedTargetFuel(posToCount: ListMap[Int, Int], posToPosDist: Map[Int, Map[Int, Int]]): (Int, Int) = {
    var (target, minFuel) = (Integer.MAX_VALUE, Integer.MAX_VALUE)
    posToPosDist.foreach { case (pos, distMap) =>
      val fuelNeeded = distMap.map { case (rp, dist) =>
        val cnt = posToCount.getOrElse(rp, 0)
        dist * cnt
      }.sum
//      println(s"pos: $pos, fuel: $fuelNeeded")
      if (fuelNeeded < minFuel) {
        target = pos
        minFuel = fuelNeeded
      }
    }
    (target, minFuel)
  }

  private def getFuel(start: Int, end: Int): Int = {
    val dist = Math.abs(end - start)
    (dist + 1) * dist / 2
  }
}
