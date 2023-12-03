import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.io.Source
import scala.util.Using

object day1 {
  @main def part1(): Unit = {
    val instructions = Util.parse("input1.txt") ( s =>
      s.toCharArray.toList )

    println(instructions
      .foldLeft(0)((z, arr) => {
        z + wieldDigits(findFirstAndLast(arr))
      }))
  }

  /*private def parse(url: String): List[List[Char]] = {
    Using(Source.fromFile(url)) {
      src =>
        src.getLines()
          .map(r => r.toCharArray.toList)
          .toList
    }.get
  }*/

  // in the following function it may occur that not all the n digits, where n is the length of the literal of the number,
  // are dropped. This because the last letter could be the starting letter of another digit literal
  @tailrec
  private def findDigit(arr: List[Char]): (Int, List[Char]) = arr match
    case Nil => (0, Nil)
    case List('o', 'n', 'e', _*) => (1, arr.drop(2))
    case List('t', 'w', 'o', _*) => (2, arr.drop(2))
    case List('t', 'h', 'r', 'e', 'e', _*) => (3, arr.drop(4))
    case List('f', 'o', 'u', 'r', _*) => (4, arr.drop(4))
    case List('f', 'i', 'v', 'e', _*) => (5, arr.drop(3))
    case List('s', 'i', 'x', _*) => (6, arr.drop(3))
    case List('s', 'e', 'v', 'e', 'n', _*) => (7, arr.drop(4))
    case List('e', 'i', 'g', 'h', 't', _*) => (8, arr.drop(4))
    case List('n', 'i', 'n', 'e', _*) => (9, arr.drop(3))
    case List(s, _*) if s.toInt >= 48 && s.toInt <= 57 => (s.toInt - 48, arr.drop(1))
    case _ => findDigit(arr.tail)

  private def findFirstAndLast(arr: List[Char]): (Int, Int) = {
    @tailrec
    def findLast(last: Int, arr: List[Char]): Int = {
      findDigit(arr) match
        case (0, Nil) => last
        case (newLast, newArr) => findLast(newLast, newArr)
    }

    val firstDigitAndLeftover = findDigit(arr)
    val ret = (firstDigitAndLeftover._1, findLast(firstDigitAndLeftover._1, firstDigitAndLeftover._2))
    ret
  }

  private def wieldDigits(du: (Int, Int)): Int =
    du._1 * 10 + du._2
}

