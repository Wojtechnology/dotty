object Test {
  def foo(a: Int, b: Int) = {
    a^b
  }

  def compute(until: Int): Int = {
    var s = 0;
    for (i <- 0 to until)
      s = foo(s, i)
    s
  }

  def main(args: Array[String]): Unit = println(compute(10))
}