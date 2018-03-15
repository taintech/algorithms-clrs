package com.taintech.algorithms

import scala.annotation.tailrec

object Main extends App {

  println("N: " + exercise122(2))

  def ln(d: Double): Double = math.log(d) / math.log(2)

  //Exercises 1.2-2, Answer: 44
  @tailrec
  def exercise122(n: Int): Int =
    if (n > 8 * ln(n)) n
    else exercise122(n + 1)

  //Exercises 1.2-3. Answer: 15
  @tailrec
  def exercise123(n: Int): Int =
    if (100 * n * n < math.pow(2, n)) n
    else exercise123(n + 1)

  def insertionSort(ar: Array[Int]): Array[Int] = {
    @tailrec
    def loop(i: Int, key: Int): Int =
      if (i < 0 || ar(i) < key) i + 1
      else {
        ar(i + 1) = ar(i)
        loop(i - 1, key)
      }
    for (i <- ar.indices.drop(1)) {
      val key = ar(i)
      val j   = loop(i - 1, key)
      ar(j) = key
    }
    ar
  }

  //p <= q < r
  def merge(ar: Array[Int], p: Int, q: Int, r: Int): Array[Int] = {
    val x    = q - p + 1
    val left = new Array[Int](x + 1)
    Array.copy(ar, p, left, 0, x)
    left(left.length - 1) = Int.MaxValue
    val y     = r - q
    val right = new Array[Int](y + 1)
    Array.copy(ar, q + 1, right, 0, y)
    right(right.length - 1) = Int.MaxValue
    @tailrec
    def loop(i: Int, li: Int, ri: Int): Unit =
      if (i > r) Unit
      else {
        if (left(li) <= right(ri)) {
          ar(i) = left(li)
          loop(i + 1, li + 1, ri)
        } else {
          ar(i) = right(ri)
          loop(i + 1, li, ri + 1)
        }
      }
    loop(p, 0, 0)
    ar
  }

  def mergeSort(ar: Array[Int]): Array[Int] = {
    def mergeSortLoop(ar: Array[Int], p: Int, r: Int): Unit = {
      if (p >= r) Unit
      else {
        val q = (p + r) / 2
        mergeSortLoop(ar, p, q)
        mergeSortLoop(ar, q + 1, r)
        merge(ar, p, q, r)
      }
    }
    mergeSortLoop(ar, 0, ar.length - 1)
    ar
  }

  def swap(ar: Array[Int], i: Int, j: Int): Array[Int] = {
    val temp = ar(i)
    ar(i) = ar(j)
    ar(j) = temp
    ar
  }

  def bubbleSort(ar: Array[Int]): Array[Int] = {
    for {
      i <- ar.indices
      j <- ar.length - 1 until i by -1
    } if (ar(j) < ar(j - 1)) swap(ar, j, j - 1)
    ar
  }

  def findMaxSubArray(ar: Array[Int]): (Int, Int, Int) = {
    def findMaxCrossingSubArray(low: Int, mid: Int, high: Int): (Int, Int, Int) = {
      var leftSum = Int.MinValue
      var sum     = 0
      var maxLeft = 0
      for (i <- mid to low by -1) {
        sum = sum + ar(i)
        if (sum > leftSum) {
          leftSum = sum
          maxLeft = i
        }
      }
      var rightSum = Int.MinValue
      sum = 0
      var maxRight = 0
      for (i <- mid + 1 to high) {
        sum = sum + ar(i)
        if (sum > rightSum) {
          rightSum = sum
          maxRight = i
        }
      }
      (maxLeft, maxRight, leftSum + rightSum)
    }
    def iterate(low: Int, high: Int): (Int, Int, Int) =
      if (low == high) (low, high, ar(low))
      else {
        val mid                      = (low + high) / 2
        val left @ (_, _, leftSum)   = iterate(low, mid)
        val right @ (_, _, rightSum) = iterate(mid + 1, high)
        val cross @ (_, _, crossSum) = findMaxCrossingSubArray(low, mid, high)
        if (leftSum >= rightSum && leftSum >= crossSum) left
        else if (rightSum >= leftSum && rightSum >= crossSum) right
        else cross
      }
    iterate(0, ar.length - 1)
  }

  def findMaxSubArrayLinear(ar: Array[Int]): (Int, Int, Int) = {
    var left     = 0
    var right    = 0
    var sum      = ar(0)
    var tempSum  = 0
    var tempLeft = 0
    for (i <- ar.indices) {
      if (ar(i) > tempSum + ar(i)) {
        tempLeft = i
        tempSum = ar(i)
      } else tempSum = tempSum + ar(i)
      if (tempSum > sum) {
        sum = tempSum
        right = i
        left = tempLeft
      }
    }
    (left, right, sum)
  }

  def reverse(ar: Array[Int]): Array[Int] = {
    val n     = ar.length
    val newAr = new Array[Int](n)
    for (i <- ar.indices)
      newAr(n - i - 1) = ar(i)
    newAr
  }

}
