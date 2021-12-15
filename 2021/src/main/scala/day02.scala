import scala.io.Source

object day02 extends App {
  def etl(filePath: String): List[(String, Int)] = {
    Source
      .fromFile(filePath)
      .getLines
      .map(x => {
        val splitted = x.split(" ")
        (splitted(0), splitted(1).toInt)
      })
      .toList
  }

  def calculateDirectionsPart1(moves: List[(String, Int)]): Int = {
    val calculation =
      moves
        .groupBy(pairs => pairs._1)
        .values
        .map(pair => pair.reduce { (x, y) => (x._1, x._2 + y._2) })
        .toMap

    calculation("forward") * (calculation("down") - calculation("up"))
  }

  def calculateDirectionsNewRulesPart2(
      moves: List[(String, Int)],
      aim: Int = 0,
      depth: Int = 0,
      forward: Int = 0,
      up: Int = 0,
      down: Int = 0
  ): Int = {
    if (moves.size == 0) {
      forward * depth
    } else {
      val move = moves.head
      val updatedMoves = moves.tail
      move match {
        case move if move._1 == s"forward" => calculateDirectionsNewRulesPart2(updatedMoves, aim, depth + move._2 * aim, forward + move._2, up, down)
        case move if move._1 == s"down"    => calculateDirectionsNewRulesPart2(updatedMoves, aim + move._2, depth, forward, up, down)
        case move if move._1 == s"up"      => calculateDirectionsNewRulesPart2(updatedMoves, aim - move._2, depth, forward, up, down)
      }
    }
  }
}
