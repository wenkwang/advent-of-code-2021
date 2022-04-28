import scala.collection.mutable
import scala.io.Source

object Day12PassagePathing {

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("passage-path-input.txt")
    val lines = source.getLines().toList

    val map: Map[String, List[String]] = buildPathMap(lines)
//    println(s"path map is: $map")
//    val paths = findPaths(map)
//    println(s"paths are: \n${paths.mkString("\n")}")
//    println(s"# of paths: ${paths.size}")

    val paths2 = findPaths2(map).distinct
    println(s"paths are: \n${paths2.mkString("\n")}")
    println(s"# of paths: ${paths2.size}")
  }

  private def buildPathMap(lines: List[String]): Map[String, List[String]] = {
    lines.foldLeft(Map.empty[String, List[String]]) { case (acc, line) =>
      val edge: Array[String] = line.split("-")
      val neighbors1: List[String] = acc.getOrElse(edge(0), List.empty[String]) :+ edge(1)
      val neighbors2: List[String] = acc.getOrElse(edge(1), List.empty[String]) :+ edge(0)
      if (edge(0) == "start" || edge(1) == "end") acc + (edge(0) -> neighbors1)
      else acc + (edge(0) -> neighbors1, edge(1) -> neighbors2)
    }
  }

  private def findPaths(map: Map[String, List[String]]): List[List[String]] = {
    val paths = mutable.ListBuffer.empty[List[String]]

    def dfs(map: Map[String, List[String]], visited: Set[String], path: List[String], pos: String): Unit = {
      if (pos == "end") paths.addOne(path)
      else {
        val nps = map.getOrElse(pos, List.empty[String]).filterNot(visited.contains)
        nps.foreach { p =>
          if (isSmallCave(p)) dfs(map, visited + p, path :+ p, p)
          else dfs(map, visited, path :+ p, p)
        }
      }
    }

    dfs(map, Set("start"), List("start"), "start")
    paths.toList
  }

  private def findPaths2(map: Map[String, List[String]]): List[List[String]] = {
    val paths = mutable.ListBuffer.empty[List[String]]

    def dfs(map: Map[String, List[String]], visited: Set[String], path: List[String], pos: String, extra: Boolean): Unit = {
//      println(s"visited: $visited, path: $path, pos: $pos, extra: $extra")
      if (pos == "end") paths.addOne(path)
      else {
        val nps = map.getOrElse(pos, List.empty[String]).filterNot(visited.contains)
        nps.foreach { p =>
          if (isSmallCave(p)) {
            if (extra) dfs(map, visited + p, path :+ p, p, true)
            else {
              dfs(map, visited, path :+ p, p, true)
              dfs(map, visited + p, path :+ p, p, false)
            }
          } else dfs(map, visited, path :+ p, p, extra)
        }
      }
    }

    dfs(map, Set("start"), List("start"), "start", false)
    paths.toList
  }

  private def isSmallCave(str: String): Boolean = str.toList.forall(_.isLower)

  private def isStartEndCave(str: String): Boolean = Set("start", "end").contains(str)

}
