import scala.annotation.targetName

object day2 {
  class RGB(val r: Int, val g: Int, val b: Int) {
    @targetName("other")
    def +(that: RGB): RGB = {
      new RGB(r + that.r, g + that.g, b + that.b)
    }

    @targetName("other")
    def <=(that: RGB): Boolean = {
      r <= that.r && g <= that.g && b <= that.b
    }

    def raise(that: RGB): RGB =
      new RGB(
        if (r > that.r) then r else that.r,
        if (g > that.g) then g else that.g,
        if (b > that.b) then b else that.b,
      )

    def power(): Int = r * g * b

    override def toString: String = s"red: $r, green: $g, blue: $b"
  }

  @main def solveDay2(): Unit = {
    val parsed: Seq[Array[RGB]] = parsedInput
    println(part1(parsed))
    println(part2(parsed))
  }

  private def part1(parsed: Seq[Array[RGB]]): Int = {
    val hypotheticalCase: RGB = RGB(12, 13, 14)


    //parsed.foreach(game => game.foreach(subGame => println(subGame)))
    parsed
      .zipWithIndex
      .map((subGames, game) =>
        if subGames.forall(sg => sg <= hypotheticalCase) then game + 1 else 0).sum
  }

  private def part2(parsed: Seq[Array[RGB]]): Int = {
    parsed.map(game => game
      .foldLeft(new RGB(0, 0, 0))((z, subGameRGB) => z
        .raise(subGameRGB))
        .power())
      .sum
  }

  private val parsedInput = Util.parse("input2.txt")(line => {
    def parseNum(arr: Array[Char]): Int =
      arr.mkString.toInt

    def extractColors(strings: Array[Array[Char]]): RGB =
      strings.map {
        case str@Array('d', 'e', 'r', ' ', _*) => RGB(parseNum(str.drop(4).reverse), 0, 0)
        case str@Array('n', 'e', 'e', 'r', 'g', ' ', _*) => RGB(0, parseNum(str.drop(6).reverse), 0)
        case str@Array('e', 'u', 'l', 'b', ' ', _*) => RGB(0, 0, parseNum(str.drop(5).reverse))
        case _ => RGB(0, 0, 0)
      }.reduce((a, b) => {
        a + b
      })

    val regex = "(Game) [0-9]+: "
    line
      .drop(regex.r.findFirstMatchIn(line).get.end)
      .split("; ")
      .map(subGame =>
        extractColors(subGame.split(", ").map(cubes => cubes.toCharArray.reverse)))
  })
}
