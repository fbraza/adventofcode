import scala.io.Source

object sonarSweep extends {
  /**
    * Function used to prepare the data
    *
    * @param filePath
    * @param windowsSize
    * @return
    */
  def etl(filePath: String, windowsSize: Int): Iterator[Seq[Int]]=
    Source
      .fromFile(filePath)
      .getLines
      .toSeq
      .map(x => x.toInt)
      .sliding(windowsSize)

  /**
    * Function used to answer the first part of day 1
    *
    * @param data
    * @return
    */
  def countHigherValuesPart1(data: Iterator[Seq[Int]]): Int =
      data
        .map(list => { if (list(1) > list(0)) 1 else 0 })
        .sum

  /**
    * Function used to answer the second part of day 1
    *
    * @param data
    * @param windowSize
    * @return
    */
  def countHigherSumPart2(data: Iterator[Seq[Int]], windowSize: Int): Int =
      data
        .map(list => list.sum)
        .sliding(windowSize)
        .map(list => { if (list(1) > list(0)) 1 else 0 })
        .sum
}

