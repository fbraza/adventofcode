import scala.io.Source

object day06 extends App {
  val PART1 = 80
  val PART2 = 256

  def etl(path: String):Array[BigInt] = {
    val ages = Source
      .fromFile(path)
      .getLines()
      .toList
      .head
      .split(",")
      .map(day => day.toInt)

    val idxAsDays = Array.fill(9)(BigInt(0))
    for (age <- ages) idxAsDays(age) += 1
    idxAsDays 
  }

  def countFish(to: Int, agesCount: Array[BigInt]): Array[BigInt] = {
    if (to > 0) {
      val curr = agesCount.head
      agesCount(7) += curr // started with 6 but because of the rotation the element switch to early to idx 5
      val update = agesCount.tail :+ curr
      println(update)
      countFish(to - 1, update)
    } else {
      return agesCount
    }
  }
}