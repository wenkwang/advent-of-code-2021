object Day21DiracDice {

  def main(args: Array[String]): Unit = {
//    val res = PracticeDice.play(2, 1)
//    println(s"winner score: ${res.winnerScore}, loser score: ${res.loserScore}, # of rounds: ${res.roundCount}. result: ${res.loserScore * res.roundCount}")

    QuantumDice.playQuantumDice(2, 1)
    println(s"# of wins for player1: ${QuantumDice.winCount1}, for player2: ${QuantumDice.winCount2}")
  }

}

case class GameResult(winnerScore: Int, loserScore: Int, roundCount: Int)

object PracticeDice {
  private val ROUND_LIMIT = 10000
  private var score1 = 0
  private var score2 = 0
  private var roundCount = 0

  private def isEnd = score1 >= 21 || score2 >= 21

  private def getWinnerScore: Option[Int] = if (isEnd) Some(Math.max(score1, score2)) else None

  private def getLoserScore: Option[Int] = if (isEnd) Some(Math.min(score1, score2)) else None

  def play(startPos1: Int, startPos2: Int): GameResult = {
    var pos1 = startPos1
    var pos2 = startPos2
    while (!isEnd && roundCount < ROUND_LIMIT) {
      val (p1, p2) = playOneRound(pos1, pos2)
      pos1 = p1
      pos2 = p2
    }
    GameResult(getWinnerScore.get, getLoserScore.get, roundCount)
  }

  private def playOneRound(pos1: Int, pos2: Int): (Int, Int) = {
    val s1 = (getNextSum + pos1 - 1) % 10 + 1
    roundCount += 3
    score1 += s1
    if (!isEnd) {
      val s2 = (getNextSum + pos2 - 1) % 10 + 1
      roundCount += 3
      score2 += s2
      (s1, s2)
    } else (s1, pos2)

  }

  private var nextDice = 0

  private def getNextSum: Int = {
    val d1 = nextDice + 1
    val d2 = (nextDice + 1) % 100 + 1
    val d3 = (nextDice + 2) % 100 + 1
    nextDice = (nextDice + 3) % 100
    d1 + d2 + d3
  }
}

object QuantumDice {

  private val diceValues = List(3, 4, 5, 6, 7, 8, 9)
  private val frequency = Map(
    3 -> 1,
    4 -> 3,
    5 -> 6,
    6 -> 7,
    7 -> 6,
    8 -> 3,
    9 -> 1
  )

  private val WIN_SCORE = 21
  var winCount1: Long = 0L
  var winCount2: Long = 0L

  def playQuantumDice(pos1: Int, pos2: Int): Unit = {
    diceValues.foreach( value =>
      playRecursively(pos1, pos2, 0, 0, List(value), 1)
    )
  }

  private def playRecursively(pos1: Int, pos2: Int, score1: Int, score2: Int, values: List[Int], turn: Int): Unit = {
//    if (winCount1 % 1000000000000L == 0) println(s"pos1: $pos1, pos2: $pos2, score1: $score1, score2: $score2, winCnt1: $winCount1, winCnt2: $winCount2")
    if (turn == 1) {
      val p1 = (pos1 + values.head - 1) % 10 + 1
      if (score1 + p1 >= WIN_SCORE) updateWinCount(values, turn)
      else diceValues.foreach(value => playRecursively(p1, pos2, score1 + p1, score2, value +: values, 2))
    } else {
      val p2 = (pos2 + values.head - 1) % 10 + 1
      if (score2 + p2 >= WIN_SCORE) updateWinCount(values, turn)
      else diceValues.foreach(value => playRecursively(pos1, p2, score1, score2 + p2, value +: values, 1))
    }
  }

  private def updateWinCount(values: List[Int], turn: Int): Unit = {
    val cnt: Long = values.foldLeft(1L)((acc, v) => acc * frequency(v))
    if (turn == 1) winCount1 += cnt
    else winCount2 += cnt
  }
}
