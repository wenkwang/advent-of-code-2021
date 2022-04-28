import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day14PolymerInsertion {

  case class Node(name: String)

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("polymer-insert-input.txt")
    val lines = source.getLines().toList.filterNot(_.trim.isEmpty)
    val template = lines.head
    val rules = lines.tail.map(parseRule).toMap

//    for (x <- 1 to 10) {
////      println(s"step $x: $template")
//      template = transform(template)
//    }
//    val charToCount: List[(Char, Int)] = template.toList.groupBy(c => c).transform((_, ls) => ls.size).toList.sortBy(_._2)
//    val mostElement = charToCount.last
//    val leastElement = charToCount.head
//    println(s"polymer result after 10 steps: \n$template")
//    println(s"most element is ${mostElement._1}, count is ${mostElement._2}")
//    println(s"least element is ${leastElement._1}, count is ${leastElement._2}")
//    println(s"subtract result: ${mostElement._2 - leastElement._2}")

    val tc = new GraphSearch(rules, 40)
    val counts = tc.transformAndCount(template)
    println(s"counts: ${counts.toList}")
  }

  private def transform(template: String, rules: Map[String, Char]): String = {
    var index = 1
    var inputStr = template
    while (index < inputStr.length) {
      val input = inputStr.substring(index - 1, index + 1)
      inputStr = rules.get(input).fold(inputStr) { n =>
        val newInputStr = inputStr.take(index) + n + inputStr.drop(index)
        index += 1
        newInputStr
      }
      index += 1
    }
    inputStr
  }

  private def parseRule(raw: String): (String, Char) = {
    val pair = raw.split(" -> ")
    (pair(0), pair(1).charAt(0))
  }

}

case class Node(name: String)

class GraphSearch(rules: Map[String, Char], depth: Int) {

  private val allNodes: Map[String, Node] = {
    val nodes = for {
      i <- 'A' to 'Z'
      j <- 'A' to 'Z'
    } yield {
      val key = i + "" + j
      key -> Node(key)
    }
    nodes.toMap
  }

  private val nodeGraph: Map[Node, mutable.ListBuffer[Node]] = {
    rules.foldLeft(Map.empty[Node, mutable.ListBuffer[Node]]) { case (acc, rule) =>
      val node = allNodes(rule._1)
      val neighbors = acc.getOrElse(node, mutable.ListBuffer.empty[Node])
      neighbors.addOne(allNodes(rule._1.take(1) + rule._2))
      neighbors.addOne(allNodes(rule._2 + rule._1.drop(1)))
      acc + (node -> neighbors)
    }
  }

  private val memorizedCounts: Map[Node, mutable.Map[Int, Array[Long]]] = allNodes.map { case (_, node) =>
    node -> mutable.Map.empty[Int, Array[Long]]
  }

  def transformAndCount(initStr: String): Array[Long] = {
    val counts: Array[Long] = Array.fill(26)(0L)
    initStr.toList.foreach(c => counts(c - 'A') += 1)
    countLetters(toNode(initStr), counts, depth)
  }

  private def countLetters(nodes: List[Node], initCounts: Array[Long], depth: Int): Array[Long] =
    nodes.foldLeft(initCounts) { case (acc, node) =>
      val cnt = countForSingleNode(node, depth)
      for (x <- acc.indices) (acc(x) += cnt(x))
      acc
    }

  private def countForSingleNode(node: Node, depth: Int): Array[Long] = {
    if (memorizedCounts(node).contains(depth)) memorizedCounts(node)(depth)
    else {
      val nc = Array.fill(26)(0L)
      nc(rules(node.name) - 'A') += 1
      if (depth == 1) {
        memorizedCounts(node).put(depth, nc)
        nc
      } else if (nodeGraph.contains(node)) {
        val count = nodeGraph(node).foldLeft(nc) { case (acc, neighbor) =>
          val cnt = countForSingleNode(neighbor, depth - 1)
          for (x <- acc.indices) (acc(x) += cnt(x))
          acc
        }
        memorizedCounts(node).put(depth, count)
        count
      } else Array.fill(26)(0L)
    }
  }

  private def toNode(str: String): List[Node] = {
    val nodes = for (x <- 1 until str.length) yield {
      allNodes(str.substring(x - 1, x + 1))
    }
    nodes.toList
  }

}
