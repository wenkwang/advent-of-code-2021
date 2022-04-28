import java.util.OptionalInt
import scala.annotation.tailrec
import scala.io.Source

object Bingo {
  def main(args: Array[String]): Unit = {
    val fileName = "bingo-input.txt"
    val source = Source.fromResource(fileName)
    val lines = source.getLines().toList
    source.close()
    val numbers = lines.head.split(",").map(_.toInt).toList
    val boards = buildBoards(lines.tail.filter(_.trim.nonEmpty), List.empty[Board])
//    val firstWinBoard = findFirstWin(boards, numbers)
//    println(s"${firstWinBoard.fold("")(b => b.numbers.toList.map(row => row.toList.mkString(", ")).mkString("\n"))}")
//    println(s"${firstWinBoard.fold("")(b => b.visited.toList.map(row => row.toList.mkString(", ")).mkString("\n"))}")
//    println(s"first win board score is: ${firstWinBoard.fold(-1)(_.getScore)}")
    val lastWinBoard = findLastWin(boards, numbers, Set.empty[Board], boards.size)
    println(s"${lastWinBoard.fold("")(b => b.numbers.toList.map(row => row.toList.mkString(", ")).mkString("\n"))}")
    println(s"${lastWinBoard.fold("")(b => b.visited.toList.map(row => row.toList.mkString(", ")).mkString("\n"))}")
    println(s"first win board score is: ${lastWinBoard.fold(-1)(_.getScore)}")
  }

  @tailrec
  private def buildBoards(lines: List[String], acc: List[Board]): List[Board] = lines match {
    case Nil => acc
    case x if x.size < 5 => acc
    case x =>
      val board = Board(x.take(5).map(_.split(" ").filter(_.trim.nonEmpty).map(_.toInt)).toArray)
      buildBoards(x.drop(5), acc :+ board)
  }

  @tailrec
  private def findFirstWin(boards: List[Board], numbers: List[Int]): Option[Board] = numbers match {
    case Nil => None
    case head :: tail => boards.filter(b => b.markNumber(head) && b.isBingo) match {
      case x if x.isEmpty => findFirstWin(boards, tail)
      case list => Some(list.head)
    }
  }

  @tailrec
  private def findLastWin(boards: List[Board], numbers: List[Int], set: Set[Board], totalNum: Int): Option[Board] = numbers match {
    case head :: tail =>
      val (winBoards, restBoards) = boards.partition(b => b.markNumber(head) && b.isBingo)
      val newSet = set ++ winBoards
      if (newSet.size == totalNum) Some(winBoards.head)
      else findLastWin(restBoards, tail, newSet, totalNum)
    case _ => None

  }
}

case class Board(numbers: Array[Array[Int]]) {
  require(numbers.length == 5 && numbers.forall(_.length == 5))

  private val numToPos: Map[Int, List[(Int, Int)]] = numbers.zipWithIndex.foldLeft(Map.empty[Int, List[(Int, Int)]]) { case (acc, row) =>
    row._1.zipWithIndex.foldLeft(acc) { case (a, num) =>
      val pos = acc.getOrElse(num._1, List.empty[(Int, Int)])
      a + (num._1 -> (pos :+ (row._2, num._2)))
    }
  }
  val visited: Array[Array[Boolean]] = Array.fill(5)(Array.fill(5)(false))

  private var rowToUnVisited: Map[Int, Set[Int]] = {
    val step1: Seq[(Int, Int, Int)] = for {
      x <- numbers.indices
      y <- numbers(x).indices
    } yield (x, y, numbers(x)(y))
    step1.foldLeft(Map.empty[Int, Set[Int]]) { case (acc, num) =>
      val set = acc.getOrElse(num._1, Set.empty[Int])
      acc + (num._1 -> (set + num._3))
    }
  }
  private var colToUnVisited: Map[Int, Set[Int]] = {
    val step1: Seq[(Int, Int, Int)] = for {
      x <- numbers.indices
      y <- numbers(x).indices
    } yield (x, y, numbers(x)(y))
    step1.foldLeft(Map.empty[Int, Set[Int]]) { case (acc, num) =>
      val set = acc.getOrElse(num._2, Set.empty[Int])
      acc + (num._2 -> (set + num._3))
    }
  }
  private var lastVisited: Option[Int] = None

  def markNumber(num: Int): Boolean = {
    val pos = numToPos.get(num)
    pos match {
      case Some(x) =>
        x.foreach { p =>
          visited(p._1)(p._2) = true
          rowToUnVisited.get(p._1).foreach { set =>
            rowToUnVisited = rowToUnVisited + (p._1 -> (set - num))
          }
          colToUnVisited.get(p._2).foreach { set =>
            colToUnVisited = colToUnVisited + (p._2 -> (set - num))
          }
        }
        lastVisited = Some(num)
        true
      case None => false
    }
  }

  def isBingo: Boolean = lastVisited.fold(false)(num => numToPos.get(num).fold(false) { case pos =>
    pos.exists(p =>
      rowToUnVisited.get(p._1).forall(_.isEmpty) || colToUnVisited.get(p._2).forall(_.isEmpty))
  })

  def getScore: Int = {
    val step1 = for {
      x <- numbers.indices
      y <- numbers(x).indices
    } yield (numbers(x)(y), visited(x)(y))
    lastVisited.fold(0)(_ * step1.filterNot(_._2).map(_._1).sum)
  }
}