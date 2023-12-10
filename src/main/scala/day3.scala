import scala.annotation.tailrec

object day3 {
  private class Schema(val schema: List[Array[Char]]) {
    //private type SymbolType = Int // 0 - blank [46], 1 - number [48 - 57], 2 - symbol [anything else]
    private type Position = (Int, Int)

    def findParts(map: List[Array[Char]]): Array[(Int, Position)] =
      @tailrec
      def findPartsRec(
                        prev: Array[Char],
                        curr: Array[Char],
                        next: Array[Char],
                        leftoverMap: List[Array[Char]],
                        res: Array[(Int, Position)]): Array[(Int, Position)] = {
        def blankOrNumber(c: Char): Boolean = c.toInt == 46 || (48 <= c.toInt && c.toInt <= 57)

        val numberRegex = "[0-9]+".r
        val matches = numberRegex.findAllIn(curr.mkString(""))
        //matches.toList.foreach(s => println(s))
        //println(s"matches: ${matches.matchData.length}, nums: ${matches.zip(matches.matchData.map(md => new Position(md.start, md.end)))}")
        val foundNumbers: Array[(Int, Position)] = matches.matchData
          .map(m => (m.matched.toInt, new Position(m.start, m.end))).toArray
          .filter((n, pos: Position) => !(prev.slice(pos._1-1, pos._2+1).forall(c => blankOrNumber(c))
            && curr.slice(pos._1-1, pos._2+1).forall(c => blankOrNumber(c))
            && next.slice(pos._1-1, pos._2+1).forall(c => blankOrNumber(c))))
        //foundNumbers.foreach(n => println(n))
        leftoverMap match
          case x :: xs => findPartsRec(curr, next, x, xs, res ++ foundNumbers)
          case List() => res ++ foundNumbers
      }

      findPartsRec(map.head, map.tail.head, map.tail.tail.head, map.tail.tail.tail, Array())
  }

  @main def solveDay3(): Unit = {
    val schema: Schema = new Schema(Array.fill(142) {
      '.'
    } +: Util.parse("input3.txt")(s => {
      '.' +: s.toCharArray :+ '.'
    }).toList :+ Array.fill(142) {
      '.'
    })

    val res = schema.findParts(schema.schema)
    println(res.map(p => p._1).sum)
    //println(schema.symbolAt(0, 0))
  }

  def part1(): Unit = {

  }
}
