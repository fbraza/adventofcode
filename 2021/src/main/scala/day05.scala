import scala.io.Source

object day05 extends App {
  def etl(path: String): Array[Array[Array[Int]]] = {
    val pattern = "\\d+,\\d+".r
    Source
      .fromFile(path)
      .getLines
      .toArray
      .map(line =>
        pattern
          .findAllIn(line)
          .toArray
          .map(coord =>
            coord
              .split(",")
              .map(point => point.toInt)
          )
      )
  }

  def countVents(data: Array[Array[Array[Int]]], size: Int = 999) = {
    val vents = Array.fill(size)(Array.fill(size)(0))
    for (line <- data) {
      if (isXsEqual(line)) {
        val row = line(0)(0)
        val y1 = line(0)(1)
        val y2 = line(1)(1)
        val colStart = if (y1 < y2) y1 else y2 
        val colEnd   = if (y1 < y2) y2 else y1
        rowUpate(vents, row, colStart, colEnd)
      } else if (isYsEqual(line)) {
        val col = line(0)(1)
        val x1 = line(0)(0)
        val x2 = line(1)(0)
        val rowStart = if (x1 < x2) x1 else x2
        val rowEnd   = if (x1 < x2) x2 else x1
        colUpdate(vents, col, rowStart, rowEnd)
      }
    }
    vents.map(line => line.count(_>=2)).sum
  }

  def rowUpate(
      table: Array[Array[Int]],
      row: Int,
      colStart: Int,
      colEnd: Int
  ): Array[Array[Int]] = {
    val updatedTable = table
    for (idx <- colStart to colEnd) { updatedTable(row)(idx) += 1 }
    updatedTable
  }

  def colUpdate(
      table: Array[Array[Int]],
      col: Int,
      rowStart: Int,
      rowEnd: Int
  ) = {
    val updatedTable = table
    for (idx <- rowStart to rowEnd) { updatedTable(idx)(col) += 1 }
    updatedTable
  }

  def diagUpdate = {} // TODO

  def isXsEqual(line: Array[Array[Int]]): Boolean = line(0)(0) == line(1)(0)
  def isYsEqual(line: Array[Array[Int]]): Boolean = line(0)(1) == line(1)(1)
}
