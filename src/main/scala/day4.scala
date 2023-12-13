object day4 {
  class ScratchCard(val winning: Array[Int], val numbers: Array[Int]) {
    assert(winning.length == 10)
    assert(numbers.length == 25)

    def points(): Array[(Int, Int)] =
      val res = for
        w <- winning
        n <- numbers
      yield (w, n)
      res
  }
}
