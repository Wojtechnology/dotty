package com.wojtechnology

import com.wojtechnology.collection.Range

object Test {
  def foo(a: Int, b: Int) = {
    a+b
  }

  def compute(until: Int): Int = {
    var s = 0;
    new Range(1, until+1, 1).foreach(i =>
      s = foo(s, i)
    )
    s
  }

  def main(args: Array[String]): Unit = println(compute(10))
}
