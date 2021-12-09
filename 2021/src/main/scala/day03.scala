import scala.io.Source
import scala.annotation.tailrec

object binaryDiagnostic extends App {
  def open(path: String): List[String] =
    Source
      .fromFile(path)
      .getLines
      .toList

  def binaryPart1(input: List[String], epsilon: String = "", gamma: String = "", idx: Int = 0): Int = {
    if (idx == input(0).size) return Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)
    val binary = input.map(x => x.charAt(idx).toString).reduce((x, y) => x + y)
    if (binary.count(_ == '1') > binary.count(_ == '0')) {
      binaryPart1(input, epsilon + "0", gamma + "1", idx + 1)
    } else {
      binaryPart1(input, epsilon + "1", gamma + "0", idx + 1)
    }
  }

  case class Oxygen()
  case class Co2()
  def binaryPart2(input: List[String], idx: Int = 0, gaz: Any): Int = {
    if (input.size == 1) return Integer.parseInt(input(0), 2)
    val binary = input.map(x => x.charAt(idx).toString).reduce((x, y) => x + y)
    gaz match {
      case Oxygen => {
        if (binary.count(_ == '1') >= binary.count(_ == '0')) {
          binaryPart2(input.filter(x => x.charAt(idx) == '1'), idx + 1, Oxygen)
        } else  {
          binaryPart2(input.filter(x => x.charAt(idx) == '0'), idx + 1, Oxygen)
        }
      }
      case Co2 => {
        if (binary.count(_ == '1') >= binary.count(_ == '0')) {
          binaryPart2(input.filter(x => x.charAt(idx) == '0'), idx + 1, Co2)
        } else  {
          binaryPart2(input.filter(x => x.charAt(idx) == '1'), idx + 1, Co2)
        }
      }
      case _ => throw new Error("gaz needs to be of type Oxygen or Co2")
    }
  }

  val test = open("day03/input02.txt")
  val result1 = binaryPart2(test, gaz = Oxygen)
  val result2 = binaryPart2(test, gaz = Co2)
  val finalResult = result1 * result2
  println(result1, result2, finalResult)
}
