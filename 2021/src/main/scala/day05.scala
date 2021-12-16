import scala.io.Source
import scala.collection.immutable.IndexedSeq

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
      if (isDiagonl(line)) {
        val x1 = line(0)(0)
        val x2 = line(1)(0)
        val y1 = line(0)(1)
        val y2 = line(1)(1)
        diagUpdate(vents, x1, x2, y1, y2)
      } else if (isXsEqual(line)) {
        val x1 = line(0)(0)
        val y1 = line(0)(1)
        val y2 = line(1)(1)
        rowUpate(vents, x1, y1, y2)
      } else if (isYsEqual(line)) {
        val y1 = line(0)(1)
        val x1 = line(0)(0)
        val x2 = line(1)(0)
        colUpdate(vents, y1, x1, x2)
      }
    }
    vents.map(line => line.count(_>=2)).sum
  }

  def rowUpate(
      table: Array[Array[Int]],
      x1: Int,
      y1: Int,
      y2: Int
  ): Array[Array[Int]] = {
    val updatedTable = table
    val colStart = if (y1 < y2) y1 else y2 
    val colEnd   = if (y1 < y2) y2 else y1
    for (idx <- colStart to colEnd) { updatedTable(idx)(x1) += 1 }
    updatedTable
  }

  def colUpdate(
      table: Array[Array[Int]],
      y1: Int,
      x1: Int,
      x2: Int
  ): Array[Array[Int]] = {
    val updatedTable = table
    val rowStart = if (x1 < x2) x1 else x2
    val rowEnd   = if (x1 < x2) x2 else x1
    for (idx <- rowStart to rowEnd) { updatedTable(y1)(idx) += 1 }
    updatedTable
  }

  def diagUpdate(
    table: Array[Array[Int]],
    x1: Int,
    x2: Int,
    y1: Int,
    y2: Int
  ): Array[Array[Int]] = {
    val updatedTable = table
    val zipIdx: IndexedSeq[(Int, Int)] = 
      if (isGoingDownRight(x1, x2, y1, y2)) x1.to(x2).zip(y1.to(y2))
      else 
        if (isGoingDownLeft(x1, x2, y1, y2)) x2.to(x1).reverse.zip(y1.to(y2))
        else 
          if (isGoingUpRight(x1, x2, y1, y2)) x1.to(x2).zip(y2.to(y1).reverse)
          else 
            if (isGoingUpLeft(x1, x2, y1, y2)) x2.to(x1).reverse.zip(y2.to(y1).reverse)
            else null
    for ((rowIdx, colIdx) <- zipIdx) {
        //println(rowIdx, colIdx)
        updatedTable(colIdx)(rowIdx) += 1
      }
    updatedTable
  }

  def isXsEqual(line: Array[Array[Int]]): Boolean = {
    val x1 = line(0)(0)
    val x2 = line(1)(0)
    x1 == x2
  }
  def isYsEqual(line: Array[Array[Int]]): Boolean = {
    val y1 = line(0)(1)
    val y2 = line(1)(1)
    y1 == y2
  }
  def isDiagonl(line: Array[Array[Int]]): Boolean = {
    val x1 = line(0)(0)
    val x2 = line(1)(0)
    val y1 = line(0)(1)
    val y2 = line(1)(1)
    (x1 - x2).abs == (y1 - y2).abs
  }

  def isGoingDownRight(x1: Int, x2: Int, y1: Int, y2: Int) = x1 < x2 && y1 < y2
  def isGoingDownLeft(x1: Int, x2: Int, y1: Int, y2: Int) = x1 > x2 && y1 < y2
  def isGoingUpRight(x1: Int, x2: Int, y1: Int, y2: Int) = x1 < x2 && y1 > y2
  def isGoingUpLeft(x1: Int, x2: Int, y1: Int, y2: Int) = x1 > x2 && y1 > y2
  
  val data = etl("data/day05/input.txt")
  val result = countVents(data)
}