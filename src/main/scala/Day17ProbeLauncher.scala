object Day17ProbeLauncher {

  def main(args: Array[String]): Unit = {
//    val pm = new ProbeMap(Pos(0, 0), Area(Pos(20, -5), Pos(30, -10)))
    val pm = new ProbeMap(Pos(0, 0), Area(Pos(241, -49), Pos(275, -75)))
    val result = pm.startSearch().distinct
    val highestPos = result.sortBy(_._1.y).takeRight(1).head._1
    println(s"highest pos: $highestPos")
    println(s"# of possible velocity: ${result.size}")
  }
}

case class Pos(x: Int, y: Int) {
  def isOnLeft(p: Pos): Boolean = x < p.x
  def isOnRight(p: Pos): Boolean = x > p.x
  def isAbove(p: Pos): Boolean = y > p.y
  def isUnder(p: Pos): Boolean = y < p.y
}

case class Area(leftTop: Pos, rightBottom: Pos) {
  def isInside(pos: Pos): Boolean =
    pos.x >= leftTop.x && pos.x <= rightBottom.x && pos.y >= rightBottom.y && pos.y <= leftTop.y

  def isOffArea(initPos: Pos, curPos: Pos): Boolean = {
    if (initPos.isOnLeft(rightBottom) && curPos.isOnRight(rightBottom)) true
    else if (initPos.isOnRight(leftTop) && curPos.isOnLeft(leftTop)) true
    else if (initPos.isAbove(rightBottom) && curPos.isUnder(rightBottom)) true
    else false
  }
}

case class Velocity (x: Int, y: Int)

class ProbeMap(start: Pos, target: Area) {

  private val LOWER_LIMIT = -1000
  private val HIGHER_LIMIT = 1000

  case class State(p: Pos, v: Velocity)

  def startSearch(): List[(Pos, Velocity)] = {
    val result = for {
      x <- 1 to target.rightBottom.x
      y <- LOWER_LIMIT to HIGHER_LIMIT
    } yield {
      val v = Velocity(x, y)
      val highestPos = oneShot(v)
      if (highestPos.isEmpty) None
      else Some((highestPos.get, v))
    }
    result.toList.flatten
  }

  private def oneShot(v: Velocity): Option[Pos] = {
//    println(s"try with velocity: [${v.x}, ${v.y}]")
    var curPos = start
    var curV = v
    var highestPos = start
    while (!target.isOffArea(start, curPos) && !target.isInside(curPos)) {
//      println(s"cur pos: (${curPos.x}, ${curPos.y})")
      val ns = nextStep(curPos, curV)
      if (ns.p.y > highestPos.y) highestPos = curPos
      curPos = ns.p
      curV = ns.v
    }
//    if (target.isInside(curPos)) println(s"in target! curPos: (${curPos.x}, ${curPos.y})")
    if (target.isInside(curPos)) Some(highestPos)
    else None
  }

  private def nextStep(p: Pos, v: Velocity): State = {
    val nx = p.x + v.x
    val ny = p.y + v.y
    val ndx = if (v.x > 0) v.x - 1 else if (v.x < 0) v.x + 1 else v.x
    val ndy = v.y - 1
    State(Pos(nx, ny), Velocity(ndx, ndy))
  }

}