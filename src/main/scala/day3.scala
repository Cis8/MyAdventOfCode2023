object day3 {
  private class Schema(val schema: Array[Array[Char]]) {
    private type SymbolType = Int // 0 - blank [46], 1 - number [48 - 57], 2 - symbol [anything else]
    private type Position = (Int, Int)
    def symbolAt(rowCol: Position): SymbolType =
      if (rowCol._1 < 0 || rowCol._1 > schema.length)
      then 0
      else if (rowCol._2 < 0 || rowCol._2 > schema(0).length)
      then 0
      else {
        val symbol = schema(rowCol._1)(rowCol._2).toInt
        if (symbol == 46) then 0
        else if (symbol >= 48 && symbol <= 57) then 1
        else 2
      }

    def findParts(): List[(Int, Position)] =
      def findNumbers(prev: Array[Char], curr: Array[Char], next: Array[Char], res: List[(Int, Position)]): List[(Int, Position)] = {
        val numberRegex = "[0-9]+".r
        numberRegex.findAllIn(curr.mkString("")).foreach(n => println(s"n ${n.}"))
        Nil
      }


      findNumbers(schema(0), schema(1), schema(2), List())
  }

  @main def solveDay3(): Unit = {
    val schema: Schema = new Schema(Array.fill(142){'.'} +: Util.parse("input3.txt")(s => {
      '.' +: s.toCharArray :+ '.'
    }).toArray :+ Array.fill(142){'.'})

    schema.findParts()
    //println(schema.symbolAt(0, 0))
  }

  def part1(): Unit = {

  }
}
