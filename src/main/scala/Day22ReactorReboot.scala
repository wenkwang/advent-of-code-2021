import scala.io.Source

case class Range(low: Int, high: Int)
case class Command(switch: Boolean, xRange: Range, yRange: Range, zRange: Range)
case class Offset(x: Int, y: Int, z: Int)

object Day22ReactorReboot {
  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("reactor-reboot-input.txt")
    val commands = source.getLines().toList.map(toCommand)
    source.close()
//    println(s"commands:\n${commands.mkString("\n")}")
    val limit = Range(-50, 50)
    val count = execute(commands, limit, limit, limit)
    val count2 = execute2(commands)
    println(s"# of cubes turned on: $count")
    println(s"# of cubes turned on: $count2")
  }

  private def execute2(commands: List[Command]): Long = {
    val (reactor, offset) = buildReactor(commands)
    for {
      x <- reactor.indices
      y <- reactor(x).indices
      z <- reactor(y).indices
    } {
      runCommands(reactor, x, y, z, offset, commands)
    }
//    println("#####-2-#####")
//    printArray(reactor)
//    println("##########")
    reactor.map(_.map(_.sum.toLong).sum).sum
  }

  private def runCommands(reactor: Array[Array[Array[Int]]], x: Int, y: Int, z: Int, offset: Offset, commands: List[Command]): Unit = {
    val nx = x + offset.x
    val ny = y + offset.y
    val nz = z + offset.z
    commands.foreach { cmd =>
      if (inRange(nx, ny, nz, cmd)) {
        if (cmd.switch) reactor(x)(y)(z) = 1
        else reactor(x)(y)(z) = 0
      }
    }
  }

  private def printArray(a: Array[Array[Array[Int]]]): Unit = {
    println(s"${a.map(_.map(_.mkString(",")).mkString("-")).mkString("\n")}")
  }

  private def inRange(x: Int, y: Int, z: Int, cmd: Command): Boolean = {
    x >= cmd.xRange.low && x <= cmd.xRange.high && y >= cmd.yRange.low && y <= cmd.yRange.high && z >= cmd.zRange.low && z <= cmd.zRange.high
  }

  private def buildReactor(commands: List[Command]): (Array[Array[Array[Int]]], Offset) = {
    val xl = commands.map(_.xRange.low).min
    val xh = commands.map(_.xRange.high).max
    val yl = commands.map(_.yRange.low).min
    val yh = commands.map(_.yRange.high).max
    val zl = commands.map(_.zRange.low).min
    val zh = commands.map(_.zRange.high).max
    val reactor = Array.fill(xh - xl + 1)(Array.fill(yh - yl + 1)(Array.fill(zh - zl + 1)(0)))
    val offset = Offset(xl, yl, zl)
    println(s"offset: $offset")
    println(s"x: ($xl,$xh), y: ($yl,$yh), z: ($zl,$zh)")
    (reactor, offset)
  }

  private def execute(commands: List[Command], xRange: Range, yRange: Range, zRange: Range): Int = {
    val offset = 50
    val reactor = Array.fill(101)(Array.fill(101)(Array.fill(101)(0)))
    commands.foreach { cmd =>
      for {
        x <- Math.max(cmd.xRange.low, xRange.low) to Math.min(cmd.xRange.high, xRange.high)
        y <- Math.max(cmd.yRange.low, yRange.low) to Math.min(cmd.yRange.high, yRange.high)
        z <- Math.max(cmd.zRange.low, zRange.low) to Math.min(cmd.zRange.high, zRange.high)
      } {
        val dx = x + offset
        val dy = y + offset
        val dz = z + offset
        if (cmd.switch) reactor(dx)(dy)(dz) = 1
        else reactor(dx)(dy)(dz) = 0
      }
    }
//    println("#####-1-#####")
//    printArray(reactor)
//    println("##########")
    reactor.map(_.map(_.sum).sum).sum
  }

  private def toCommand(raw: String): Command = {
    val parts = raw.split(" ")
    val switch = if (parts(0) == "on") true else false
    val ranges = parts(1).split(",").map { r =>
      val d = r.charAt(0)
      val range = r.substring(2).split("\\.\\.")
      d -> Range(range(0).toInt, range(1).toInt)
    }.toMap
    Command(switch, ranges('x'), ranges('y'), ranges('z'))
  }

}
