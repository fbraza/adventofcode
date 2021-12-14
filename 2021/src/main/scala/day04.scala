import scala.io.Source
import scala.collection.mutable

object giantSquid extends App {
  val NEGATOR = -999999
  val INIMEMO = memo(etl("data/day04/input.txt")._1)

  def etl(path: String): (List[Int], List[List[List[Int]]]) = {
    val data = Source
      .fromFile("data/day04/input.txt")
      .getLines
      .toList
    val numbers = data(0)
      .split(",")
      .map(digit => digit.toInt)
      .toList
    val boards = data
      .filter(x => x.size <= 14 && x != "")
      .map(row =>
        row.split(" ").filter(ele => ele != "").toList.map(ele => ele.toInt)
      )
      .sliding(5, 5)
      .toList
    (numbers, boards)
  }

  def findFirstWinningBoard(
      numbers: List[Int],
      boards: List[List[List[Int]]],
      memo: mutable.Map[Int, Boolean] = INIMEMO,
      previousNumber: Int = 0
  ): Int = {
    val currentNumber = numbers(0)
    val updatedMemo = memo
    val updatedNumbers = numbers.drop(1)
    val winningBoard = boards.filter(board => isBingo(board))
    if (winningBoard.size > 0)
      return sumBoard(winningBoard(0)) * previousNumber
    val updatedBoards = update(boards, updatedMemo, currentNumber)
    findFirstWinningBoard(updatedNumbers, updatedBoards, updatedMemo, currentNumber)
  }

  def findLastWinningBoard(
      numbers: List[Int],
      boards: List[List[List[Int]]],
      memo: mutable.Map[Int, Boolean] = INIMEMO,
      previousNumber: Int = 0
  ): Int = {
    val currentNumber = numbers(0)
    val updatedMemo = memo
    val updatedNumbers = numbers.drop(1)
    if (boards.size == 1 && boards.exists(board => isBingo(board)))
      return sumBoard(boards(0)) * previousNumber
    val notWinningBoards = boards.filter(board => !isBingo(board))
    val updatedBoards = update(notWinningBoards, updatedMemo, currentNumber)
    findLastWinningBoard(updatedNumbers, updatedBoards, updatedMemo, currentNumber)
  }

  def isBingo(board: List[List[Int]]): Boolean = {
    val areRowBingo = board.exists(row => row.forall(num => num < 0))
    val areColBingo = board.transpose.exists(row => row.forall(num => num < 0))
    areRowBingo || areColBingo
  }

  def sumBoard(board: List[List[Int]]): Int = {
    board
      .map(row => row.filter(num => num > 0).sum)
      .sum
  }

  def memo(numbers: List[Int]): mutable.Map[Int, Boolean] = {
    val memo = mutable.Map[Int, Boolean]()
    for (number <- numbers) { memo(number) = false }
    memo
  }

  def update(
      boards: List[List[List[Int]]],
      memo: mutable.Map[Int, Boolean],
      current: Int
  ):List[List[List[Int]]] = {
    val result = boards
      .map(board =>
        board.map(row =>
          row.map(num => {
            if (num < 0) {
              num
            } else if (memo(num)) {
              num + NEGATOR
            } else if (!memo(num) && num == current) {
              memo(num) = true
              num + NEGATOR
            } else {
              num
            }
          })
        )
      )
    result
  }
}
