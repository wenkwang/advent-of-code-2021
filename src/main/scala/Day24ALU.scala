import scala.collection.mutable
import scala.io.Source

object Day24ALU {

  object Command {
    def buildCommand(raw: String): Command = {
      val vec = raw.split(" ")
      vec(0) match {
        case "inp" => Input(vec(1))
        case "add" => Add(vec(1), vec(2))
        case "mul" => Multiply(vec(1), vec(2))
        case "div" => Divide(vec(1), vec(2))
        case "mod" => Mod(vec(1), vec(2))
        case "eql" => Equal(vec(1), vec(2))
      }
    }
  }

  trait Command
  case class Input(v: String) extends Command
  case class Add(v1: String, v2: String) extends Command
  case class Multiply(v1: String, v2: String) extends Command
  case class Divide(v1: String, v2: String) extends Command
  case class Mod(v1: String, v2: String) extends Command
  case class Equal(v1: String, v2: String) extends Command


  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("alu-monad-input.txt")
    val commands = source.getLines().toList.map(Command.buildCommand)
    val processor = buildProcessor(commands)
//    var flag = 9
//    for {
//      s1 <- 9 to 1 by -1
//      s2 <- 9 to 1 by -1
//      s3 <- 9 to 1 by -1
//      s4 <- 9 to 1 by -1
//      s5 <- 9 to 1 by -1
//      s6 <- 9 to 1 by -1
//      s7 <- 9 to 1 by -1
//      s8 <- 9 to 1 by -1
//      s9 <- 9 to 1 by -1
//      s10 <- 9 to 1 by -1
//      s11 <- 9 to 1 by -1
//      s12 <- 9 to 1 by -1
//    } {
//      val input1 = Vector(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, 1, 7)
//      val input2 = Vector(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, 2, 8)
//      val input3 = Vector(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, 3, 9)
//      val res1 = processor(input1)
//      val res2 = processor(input2)
//      val res3 = processor(input3)
//      if (flag != s1) {
//        println(s"starting with $s1 now.")
//        flag = s1
//      }
//      if (res1 == 0) println(s"model number: $input1")
//      if (res2 == 0) println(s"model number: $input2")
//      if (res3 == 0) println(s"model number: $input3")
//    }
    val testInput = Vector(9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 1, 7)
    val testRes = processor(testInput)
    println(s"model number: $testRes")

  }

  private def buildProcessor(cmds: List[Command]): (Vector[Int] => Long) = (num: Vector[Int]) => {
    val vMap = mutable.Map("w" -> 0L, "x" -> 0L, "y" -> 0L, "z" -> 0L)
    var index = 0
    cmds.foreach {
      case Input(p1) =>
        vMap.put(p1, num(index))
        index += 1
      case Add(p1, p2) => {
        if (vMap.contains(p2)) vMap.put(p1, vMap(p1) + vMap(p2))
        else vMap.put(p1, vMap(p1) + p2.toInt)
      }
      case Multiply(p1, p2) => {
        if (vMap.contains(p2)) vMap.put(p1, vMap(p1) * vMap(p2))
        else vMap.put(p1, vMap(p1) * p2.toInt)
      }
      case Divide(p1, p2) =>
        if (vMap.contains(p2)) vMap.put(p1, vMap(p1) / vMap(p2))
        else vMap.put(p1, vMap(p1) / p2.toInt)
      case Mod(p1, p2) =>
        if (vMap.contains(p2)) vMap.put(p1, vMap(p1) % vMap(p2))
        else vMap.put(p1, vMap(p1) % p2.toInt)
      case Equal(p1, p2) =>
        if (vMap.contains(p2)) vMap.put(p1, if (vMap(p1) == vMap(p2)) 1 else 0)
        else vMap.put(p1, if (vMap(p1) == p2.toInt) 1 else 0)
    }
    vMap("z")
  }
}
