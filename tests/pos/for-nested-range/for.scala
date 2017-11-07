package com.wojtechnology

import com.wojtechnology.collection.Range

object Test {

  def compute(until: Int): Int = {
    var s = 0;
    new Range(1, until+1, 1).foreach(_ =>
      new Range(1, until+1, 1).foreach(_ =>
        new Range(1, until+1, 1).foreach(_ => s += 1
        )
      )
    )
    s
  }

  def main(args: Array[String]): Unit = println(compute(10))
}