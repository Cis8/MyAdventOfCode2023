import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.HashMap

object day3 {
  private class Schema(val schema: List[Array[Char]]) {
    //private type SymbolType = Int // 0 - blank [46], 1 - number [48 - 57], 2 - symbol [anything else]
    private type Position = (Int, Int)

    private def combineIterables[K, V](a: mutable.HashMap[K, List[V]], b: mutable.HashMap[K, List[V]]): mutable.HashMap[K, List[V]] = {
      a ++ b.map { case (k, v) => k -> (v ++ a.getOrElse(k, List())) }
    }

    def findParts(map: List[Array[Char]]): (Array[(Int, Position)], mutable.HashMap[Position, List[Int]]) =
      @tailrec
      def findPartsRec(
                        prev: Array[Char],
                        curr: Array[Char],
                        next: Array[Char],
                        rowNumberOfCurr: Int,
                        leftoverMap: List[Array[Char]],
                        res: Array[(Int, Position)],
                        resGearedParts: mutable.HashMap[Position, List[Int]]): (Array[(Int, Position)], mutable.HashMap[Position, List[Int]]) = {
        def blankOrNumber(c: Char): Boolean = c.toInt == 46 || (48 <= c.toInt && c.toInt <= 57)

        val numberRegex = "[0-9]+".r
        val matches = numberRegex.findAllIn(curr.mkString(""))
        val matchesCopy = numberRegex.findAllIn(curr.mkString(""))
        //matches.toList.foreach(s => println(s))
        //println(s"matches: ${matches.matchData.length}, nums: ${matches.zip(matches.matchData.map(md => new Position(md.start, md.end)))}")
        val foundNumbers: Array[(Int, Position)] = matches.matchData
          .map(m => (m.matched.toInt, new Position(m.start, m.end))).toArray
          .filter((n, pos: Position) => !(prev.slice(pos._1-1, pos._2+1).forall(c => blankOrNumber(c))
            && curr.slice(pos._1-1, pos._2+1).forall(c => blankOrNumber(c))
            && next.slice(pos._1-1, pos._2+1).forall(c => blankOrNumber(c))))

        val gearToNumsHashMap: mutable.HashMap[Position, List[Int]] = matchesCopy.matchData
          .map(m => (m.matched.toInt, new Position(m.start, m.end)))
          .foldLeft(new mutable.HashMap[Position, List[Int]]())((z, nPos) => {
            val p = prev.slice(nPos._2._1 - 1, nPos._2._2 + 1).indexWhere(c => c =='*')
            val c = curr.slice(nPos._2._1 - 1, nPos._2._2 + 1).indexWhere(c => c =='*')
            val n = next.slice(nPos._2._1 - 1, nPos._2._2 + 1).indexWhere(c => c =='*')
            if p != -1 then { val k = new Position(rowNumberOfCurr-1, p + nPos._2._2); z.update(k, nPos._1 +: z.getOrElse(k, List())); z }
            else if c != -1 then { val k = new Position(rowNumberOfCurr, c + nPos._2._2); z.update(k, nPos._1 +: z.getOrElse(k, List())); z }
            else if n != -1 then { val k = new Position(rowNumberOfCurr+1, n + nPos._2._2); z.update(k, nPos._1 +: z.getOrElse(k, List())); z }
            else z
          })

        println("---")
        gearToNumsHashMap.foreach(hm => println(s"${hm._1} -> ${hm._2}"))
        println("---")


        //foundNumbers.foreach(n => println(n))
        leftoverMap match
          case x :: xs => findPartsRec(curr, next, x, rowNumberOfCurr + 1, xs, res ++ foundNumbers, combineIterables[Position, Int](resGearedParts, gearToNumsHashMap))
          case List() => (res ++ foundNumbers, combineIterables[Position, Int](resGearedParts, gearToNumsHashMap))
      }

      findPartsRec(map.head, map.tail.head, map.tail.tail.head, 1, map.tail.tail.tail, Array(), mutable.HashMap[Position, List[Int]]())
  }

  @main def solveDay3(): Unit = {
    val schema: Schema = new Schema(Array.fill(142) {
      '.'
    } +: Util.parse("input3test.txt")(s => {
      '.' +: s.toCharArray :+ '.'
    }).toList :+ Array.fill(142) {
      '.'
    })

    val res = schema.findParts(schema.schema)
    println(res._1.map(p => p._1).sum)
    println(res._2.values.filter(posNums => posNums.size == 2).map(l => l.head * l(1)).sum)
    //println(schema.symbolAt(0, 0))
  }

  def part1(): Unit = {

  }
}
