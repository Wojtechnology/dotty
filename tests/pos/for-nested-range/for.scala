object Test {

  def compute(until: Int): Int = {
    var s = 0;
    for (i <- 1 to until)
      for (j <- 1 to until)
        for (k <- 1 to until)
          s += 1
    s
  }

  def main(args: Array[String]): Unit = println(compute(10))
}