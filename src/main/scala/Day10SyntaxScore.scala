import java.util.OptionalInt
import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day10SyntaxScore {

  val errorCharToScore = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  val charToScore = Map (
    '(' -> 1,
    '[' -> 2,
    '{' -> 3,
    '<' -> 4
  )

  val leftChars = Set('(', '[', '<', '{')

  val pairs = Map(
    '(' -> ')',
    '[' -> ']',
    '<' -> '>',
    '{' -> '}'
  )

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("syntax-score-input.txt")
    val lines = source.getLines().toList
    source.close()

    val errorChars = lines.flatMap(line => findIllegalChar(line.toList, mutable.Stack.empty[Char]))
    val errorScore = errorChars.map(errorCharToScore(_)).sum
    println(s"error score is: $errorScore")

    val inCompleteLines = lines.flatMap(line => findCompletionString(line.toList, mutable.Stack.empty[Char]))
    val inCompleteScores = inCompleteLines.map(getScore).sorted
    println(s"inCompleteScores: \n$inCompleteScores")
    val middleScore = inCompleteScores(inCompleteScores.length / 2)
    println(s"incomplete middle score is: $middleScore")
  }

  private def getScore(chars: List[Char]): Long = chars.foldLeft(0L)((acc, c) => acc * 5 + charToScore(c))

  @tailrec
  private def findCompletionString(chars: List[Char], stack: mutable.Stack[Char]): Option[List[Char]] = chars match {
    case Nil => Some(stack.toList)
    case head :: tail => {
      if (isLeft(head)) {
        stack.push(head)
        findCompletionString(tail, stack)
      } else if (stack.nonEmpty && isMatched(stack.head, head)) {
        stack.pop()
        findCompletionString(tail, stack)
      } else None
    }
  }


  @tailrec
  private def findIllegalChar(chars: List[Char], stack: mutable.Stack[Char]): Option[Char] = chars match {
    case Nil => None
    case head :: tail =>
      if (isLeft(head)) {
        stack.push(head)
        findIllegalChar(tail, stack)
      } else if (stack.nonEmpty && isMatched(stack.head, head)) {
        stack.pop()
        findIllegalChar(tail, stack)
      } else Some(head)
  }

  private def isLeft(c: Char): Boolean = leftChars.contains(c)

  private def isMatched(c1: Char, c2: Char): Boolean = if (isLeft(c1)) pairs(c1) == c2 else pairs(c2) == c1

}
