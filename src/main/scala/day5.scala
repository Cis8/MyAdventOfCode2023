import scala.annotation.tailrec
import scala.collection.SortedSet
import scala.io.Source
import scala.util.Using

object day5 {
  class Range(val a: BigInt, val b: BigInt) extends Ordered[Range] {
    def contains(n: BigInt): Boolean = n >= a && n <= b

    // precondition: n is contained in this Range
    def offset(n: BigInt): BigInt =
      if (!contains(n)) throw Exception(s"$n is not contained in range $this")
      n - a

    def length(): BigInt = b - a + 1

    override def toString: String = s"[$a, $b]"

    def overlapsBeg(that: Range): Boolean = b > that.a && b <= that.b && a < that.a

    def overlapsEnd(that: Range): Boolean = a < that.b && a >= that.a && b > that.b

    def containedIn(that: Range): Boolean = a >= that.a && b <= that.b

    def before(that: Range): Boolean = b < that.a

    def after(that: Range): Boolean = a > that.b

    def intersect(that: Range): Option[Range] =
      val maxA = if a > that.a then a else that.a
      val minB = if b < that.b then b else that.b
      // a range is returned only if a > b, otherwise it means that there is no overlap and hence no intersection
      if maxA < minB then Some(new Range(maxA, minB)) else None

    def minus(that: Range): Option[Range | (Range, Range)] =
    // if that is contained in this, return 2 subsets
      if that.containedIn(this)
      then
        if !(overlapsBeg(that) || overlapsEnd(that))
        then Option((new Range(a, that.a - 1), new Range(that.b + 1, b)))
        else if overlapsBeg(that)
        then Option(new Range(a, that.a - 1))
        else Option(new Range(that.b + 1, b))
      // if that is not overlapping this, return this
      else if before(that) || after(that)
      then Option(this.clone())
      // if that overlaps the first part of this, return the second part of this
      else if overlapsEnd(that)
      then Option(new Range(that.b + 1, b))
      // if that overlaps the second part of this, return the first part of this
      else if overlapsBeg(that)
      then Option(new Range(a, that.a - 1))
      // if this is contained in that, return None
      else None

    override def clone(): Range = new Range(a, b)

    override def compare(that: Range): Int = a.compareTo(that.a) match
      case 0 => b.compareTo(that.b)
      case n => n
  }

  // SDR stands for Source Destination Range
  private class SDR(s: BigInt, d: BigInt, r: BigInt) extends Ordered[SDR] {
    val source: Range = new Range(s, s + r - 1)
    val destination: Range = new Range(d, d + r - 1)

    // precondition: n is contained in the source Range
    def destinationFor(n: BigInt): BigInt =
      if (!source.contains(n)) throw Exception(s"$n is not contained in range $source")
      destination.a + source.offset(n)

    def narrow(r: Range): SDR =
      if !r.containedIn(source) then throw Exception(s"The input range must be contained in the source range. Tried to narrow $this with $r")
      val narrowed = SDR(r.a, destination.a + source.offset(r.a), r.length())
      narrowed

    override def compare(that: SDR): Int = {
      s.compareTo(that.source.a)
    }

    override def toString: String = s"$source -> $destination"
  }

  private class SdrMap(_sdrs: List[SDR]) {
    val sdrs: List[SDR] = _sdrs.sorted

    def mapValue(n: BigInt): BigInt = {
      sdrs.find(sdr => sdr.source.contains(n)) match
        case None => n
        case sdr: Option[SDR] => sdr.get.destinationFor(n)
    }

    override def toString: String = sdrs.mkString("SdrMap(", ", ", ")")
  }

  private def splitMaps(input: List[String]): List[SdrMap] = {
    @tailrec
    def splitMapsRec(i: List[String], res: List[SdrMap]): List[SdrMap] = {
      def extract(ll: List[String]): List[SDR] = ll.tail
        .map(line => line.split(" "))
        .map(strNums => strNums.map(strN => BigInt(strN)))
        .map(nums => SDR(nums(1), nums(0), nums(2)))

      val (a, b) = i.splitAt(i.indexWhere(s => s == ""))
      (a, b) match
        case (Nil, x) => res :+ SdrMap(extract(x))
        case (x, l) => splitMapsRec(b.tail, res :+ SdrMap(extract(x)))
    }

    splitMapsRec(input, List())
  }

  @main def solve(): Unit = {
    val parsed: List[String] = Using(Source.fromFile(s"./src/main/resources/input5.txt")) {
      src =>
        src.getLines().toList
    }.get

    part1(parsed)
    part2(parsed)
  }

  private def part1(parsed: List[String]): Unit = {
    val seeds = parsed.head.split(" ").tail.map(s => BigInt(s))
    val maps = splitMaps(parsed.tail.tail)

    val seedsDests: Array[(BigInt, BigInt)] = seeds.map(seed =>
      (seed, maps.foldLeft(seed)((z: BigInt, sdrMap: SdrMap) => sdrMap.mapValue(z)))
    )
    val chosenSeed: (BigInt, BigInt) = seedsDests minBy {
      _._2
    }
    println(s"The seed to plant is ${chosenSeed._1} at dest ${chosenSeed._2}")
  }

  private def part2(parsed: List[String]): Unit = {
    val regex = "[0-9]+ [0-9]*".r
    val seedRanges: SortedSet[Range] = SortedSet.empty[Range] ++ regex.findAllIn(parsed.head).map(s => {
      val tuple = s.split(" ").map(v => BigInt(v))
      new Range(tuple(0), tuple(0) + tuple(1) - 1)
    })
    val maps = splitMaps(parsed.tail.tail)

    def scatter(a: Range, xs: List[SDR]): SortedSet[Range] =
      if xs == List() then SortedSet.empty[Range] + a
      else
        val x1 = xs.head
        if a.before(x1.source)
        then SortedSet.empty[Range] + a
        else if a.after(x1.source)
        then scatter(a, xs.tail)
        else if a.overlapsBeg(x1.source)
        then {
          SortedSet.empty[Range] + a.minus(x1.source).get.asInstanceOf[Range] + x1.narrow(a.intersect(x1.source).get).destination
        }
        else if a.overlapsEnd(x1.source)
        then {
          SortedSet.empty[Range] + x1.narrow(a.intersect(x1.source).get).destination ++ scatter(a.minus(x1.source).get.asInstanceOf[Range], xs.tail)
        }
        else if x1.source.containedIn(a)
        then {
          val leftoverRanges: (Range, Range) = a.minus(x1.source).get.asInstanceOf[(Range, Range)]
          val result = SortedSet.empty[Range]
            + leftoverRanges._1
            + x1.narrow(a.intersect(x1.source).get).destination
            ++ scatter(leftoverRanges._2, xs.tail)
          result
        }
        else if a.containedIn(x1.source)
        then SortedSet.empty[Range] + x1.narrow(a).destination
        else SortedSet.empty[Range] + a


    val mappedRanges: SortedSet[Range] = seedRanges.flatMap(range => {
      maps.foldLeft(SortedSet.empty[Range] + range)((z: SortedSet[Range], sdrMap: SdrMap) => {
        z.flatMap((rr: Range) => scatter(rr, sdrMap.sdrs))
      })
    })

    println(s"lowest loc: ${mappedRanges.head.a}")
  }

}
