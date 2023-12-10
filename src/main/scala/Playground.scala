import scala.collection.SortedSet

object Playground {
  @main def test(): Unit = {
    val seedRanges: SortedSet[Int] = SortedSet.empty[Int] ++ Set(10, 40, 30)
    val ss: SortedSet[Int] = SortedSet.empty[Int] ++ Set(1, 7, 3, 0, 5, 6)
    val res = seedRanges.flatMap(s => ss.map(ssEl => s+ssEl))
    res.foreach(el => println(el))
  }
}
