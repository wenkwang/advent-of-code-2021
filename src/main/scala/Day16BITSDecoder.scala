import scala.io.Source

object Day16BITSDecoder {

  def main(args: Array[String]): Unit = {
    val rawCode = Source.fromResource("bits-decoder-input.txt").getLines().toList.head
    val packet = Decoder.process(rawCode)
    val value = Decoder.calculate(packet)
    val expression = Decoder.parseExpression(packet)
    println(s"=> sum of version ids: ${packet.versionSum}")
    println(s"=> expression is:\n$expression")
    println(s"=> calculation result of the packet is: ${value.toString}")
  }

}

trait Packet {
  def versionId: Long
  def typeId: Long
  def versionSum: Long
}

case class Operator(versionId: Long, typeId: Long, lengthTypeId: Long, packets: List[Packet], versionSum: Long) extends Packet

case class LiteralValue(versionId: Long, literalValue: Long) extends Packet {
  val typeId = 4L
  val versionSum = versionId
}

case class EmptyPacket() extends Packet {
  val versionId = -1
  val typeId = -1
  val versionSum = 0
}

object Decoder {

  def binaryToDecimal(binary: String): Long = binary.foldLeft(0L)((acc, c) => 2L * acc + (c - '0'))

  private val hexToBinaryDict = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )

  case class State(packet: Packet, code: String)

  def process(hexCode: String): Packet = {
    val binaryCode = hexCode.foldLeft("")((acc, c) => acc + hexToBinaryDict(c))
    println(s"binaryCode: $binaryCode")
    val state = decodeBinary(binaryCode)
    println(s"final packet: ${state.packet}")
    state.packet
  }

  def calculate(packet: Packet): BigInt = packet match {
    case v: LiteralValue => v.literalValue
    case op: Operator => op.typeId match {
      case 0 => op.packets.map(calculate).sum
      case 1 => op.packets.foldLeft(BigInt(1))((acc, p) => acc * calculate(p))
      case 2 => op.packets.map(calculate).min
      case 3 => op.packets.map(calculate).max
      case 5 =>
        val v1 = calculate(op.packets.head)
        val v2 = calculate(op.packets(1))
        if (v1 > v2) 1 else 0
      case 6 =>
        val v1 = calculate(op.packets.head)
        val v2 = calculate(op.packets(1))
        if (v1 < v2) 1 else 0
      case 7 =>
        val v1 = calculate(op.packets.head)
        val v2 = calculate(op.packets(1))
        if (v1 == v2) 1 else 0
    }
  }

  def parseExpression(packet: Packet): String = packet match {
    case v: LiteralValue => v.literalValue.toString
    case op: Operator => op.typeId match {
      case 0 => s"(${op.packets.map(parseExpression).mkString("+")})"
      case 1 => s"(${op.packets.map(parseExpression).mkString("*")})"
      case 2 => s"Min(${op.packets.map(parseExpression).mkString(",")})"
      case 3 => s"Max(${op.packets.map(parseExpression).mkString(",")})"
      case 5 =>
        val v1 = parseExpression(op.packets.head)
        val v2 = parseExpression(op.packets(1))
        s"($v1>$v2?)"
      case 6 =>
        val v1 = parseExpression(op.packets.head)
        val v2 = parseExpression(op.packets(1))
        s"($v1<$v2?)"
      case 7 =>
        val v1 = parseExpression(op.packets.head)
        val v2 = parseExpression(op.packets(1))
        s"($v1==$v2?)"
    }
  }

  private def decodeBinary(code: String): State = {
    val versionId = binaryToDecimal(code.take(3))
    val typeId = binaryToDecimal(code.slice(3, 6))
    if (typeId == 4) decodeLiteralValue(versionId, code.drop(6))
    else {
      val lengthTypeId = binaryToDecimal(code.slice(6, 7))
      if (lengthTypeId == 0) decodeOperator15Bit(versionId, typeId, code.drop(7))
      else decodeOperator11Cnt(versionId, typeId, code.drop(7))
    }
  }

  private def decodeOperator15Bit(versionId: Long, typeId: Long, code: String): State = {
//    println(s"decodeOperator15Bit: $code")
    val bitLimit = binaryToDecimal(code.take(15))
    var curLen = 0
    var rest = code.drop(15)
    var packets = List.empty[Packet]
    while (curLen < bitLimit) {
      val state = decodeBinary(rest)
      packets = packets :+ state.packet
      curLen += rest.length - state.code.length
      rest = state.code
    }
    State(Operator(versionId, typeId, 0, packets, packets.map(_.versionSum).sum + versionId), rest)
  }

  private def decodeOperator11Cnt(versionId: Long, typeId: Long, code: String): State = {
//    println(s"decodeOperator11Cnt: $code")
    val cntLimit = binaryToDecimal(code.take(11))
    var curCnt = 0
    var rest = code.drop(11)
    var packets = List.empty[Packet]
    while (curCnt < cntLimit) {
      val state = decodeBinary(rest)
      packets = packets :+ state.packet
      curCnt += 1
      rest = state.code
    }
    State(Operator(versionId, typeId, 1, packets, packets.map(_.versionSum).sum + versionId), rest)
  }

  private def decodeLiteralValue(versionId: Long, code: String): State = {
//    println(s"decodeLiteralValue: $code")
    var rest = code
    var isLast = false
    var acc = ""
    while (!isLast && rest.nonEmpty) {
      isLast = rest.take(1) == "0"
      acc += rest.slice(1, 5)
      rest = rest.drop(5)
    }
    val value = binaryToDecimal(acc)
//    if (value < 0) println(s"[WARN] value: $value, acc: $acc")
    State(LiteralValue(versionId, value), rest)
  }
}
