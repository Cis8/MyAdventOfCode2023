import scala.io.Source
import scala.util.Using

object day5 {
  private type Number = Int

  class Range(val a: Number, val b: Number) {
    override def toString: String = s"[$a , $b]"
  }

  // SDR stands for Source Destination Range
  class SDR(s: Number, d: Number, r: Number) {
    val source: Range = new Range(s, s+r)
    val destination: Range = new Range(d, d+r)
  }

  class Map(sdrs: List[SDR]) {
    val maps: List[SDR] = sdrs.sortBy()
  }

  @main def solve(): Unit = {
    val parsed: List[String] = Using(Source.fromFile(s"./src/main/resources/input5.txt")) {
      src =>
        src.getLines().toList
    }.get

    val seeds = parsed.head.split(" ").tail



    println(seeds.mkString("Seeds(", ", ", ")"))

    parsed.tail.tail.foreach(el => println(el))
  }


}
