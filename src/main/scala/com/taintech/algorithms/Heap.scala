package com.taintech.algorithms

import scala.annotation.tailrec

object Heap {

  def parent(i: Int): Int = (i - 1) / 2
  def left(i: Int): Int   = 2 * i + 1
  def right(i: Int): Int  = 2 * i + 2

  def indexOf(ar: Array[Int], i: Int): Option[Int] = if (i < ar.length) Some(ar(i)) else None

  @tailrec
  def maxHeapify(ar: Array[Int], i: Int, heapSize: Int): Array[Int] = {
    val li      = left(i)
    val ri      = right(i)
    println(s"heap-size: i: $i - $heapSize - ($li, $ri) - (${indexOf(ar, li)} , ${indexOf(ar, ri)})")
    var largest = if (li < heapSize && ar(li) > ar(i)) li else i
    largest = if (ri < heapSize && ar(ri) > ar(largest)) ri else largest
    if (largest != i) {
      Main.swap(ar, i, largest)
      maxHeapify(ar, largest, heapSize)
    } else ar
  }

  def buildMaxHeap(ar: Array[Int]): Array[Int] = {
    for (i <- ar.length/ 2 to 0 by -1)
      maxHeapify(ar, i, ar.length)
    ar
  }

  def heapSort(ar: Array[Int]): Array[Int] = {
    println(s"starting: ${ar.toList}")
    println(s"------------------------------------------------------")
    buildMaxHeap(ar)
    println(s"max-heap-array: ${ar.toList}")
    println(s"------------------------------------------------------")
    var heapSize = ar.length
    for (i <- ar.length - 1 to 1 by -1) {
      println(s"i: $i, ${ar.toList}")
      Main.swap(ar, 0, i)
      println(s"swap: ${ar.toList}")
      heapSize = heapSize - 1
      maxHeapify(ar, 0, heapSize)
      println(s"max-heapify: ${ar.toList}")
      println(s"------------------------------------------------------")
    }
    ar
  }

}
