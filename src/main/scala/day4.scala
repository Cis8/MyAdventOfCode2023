import scala.annotation.tailrec

object day4 {
  private class ScratchCard(val winning: Array[Int], val numbers: Array[Int]) {

    def winningNumbersCardinality(): Int =
      val res = for
        w <- winning
        n <- numbers
      yield (w, n)
      res.count((a, b) => a == b)

    def points(): Double =
      val cardinality = winningNumbersCardinality()
      if cardinality <= 0
      then 0
      else Math.pow(2, cardinality - 1)
  }

  @main def solveDay4(): Unit = {
    val pile: Seq[ScratchCard] = Util.parse("input4.txt")(s => {
      val columnIndex = s.indexOf(":")
      val pipeIndex = s.indexOf('|')
      val digitRegex = """\d+""".r
      ScratchCard(
        (digitRegex findAllIn s.slice(columnIndex + 1, pipeIndex - 1)).toArray.map(sv => sv.toInt), //10, 39
        (digitRegex findAllIn s.drop(pipeIndex + 1)).toArray.map(sv => sv.toInt)) //42
    })

    part1(pile)
    part2(pile)
  }

  private def part1(pile: Seq[ScratchCard]): Unit = {
    println(pile.map(sc => sc.points()).sum.toInt)
  }

  private def part2(pile: Seq[ScratchCard]): Unit = {
    def sumTimes(leftCycles: Int, list: List[Int], valueToAdd: Int): List[Int] =
      if (leftCycles == 0) then list else
        list match
          case List() => List.fill(leftCycles)(valueToAdd)
          case l => l.head + valueToAdd :: sumTimes(leftCycles - 1, l.tail, valueToAdd)

    @tailrec
    def totalCards(scratchCards: List[ScratchCard], extraCopies: List[Int], totCardsSeen: Int): Int = {
      if scratchCards.isEmpty then totCardsSeen else
        extraCopies match
          case List() => totalCards(scratchCards.tail, sumTimes(scratchCards.head.winningNumbersCardinality(), List(), 1), totCardsSeen + 1)
          case l => totalCards(scratchCards.tail, sumTimes(scratchCards.head.winningNumbersCardinality(), l.tail, l.head + 1), totCardsSeen + l.head + 1)
    }

    println(totalCards(pile.toList, List(), 0))
  }
}
