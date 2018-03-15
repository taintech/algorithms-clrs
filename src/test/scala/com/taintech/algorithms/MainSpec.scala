package com.taintech.algorithms

import org.scalatest.{ FlatSpec, Matchers }

class MainSpec extends FlatSpec with Matchers {

  "'Hello' + 'world'" should "return 'Helloworld'" in {
    "Hello" + "world" should be("Helloworld")
  }

  "Insertion sort" should "sort array 3,2,1" in {
    Main.insertionSort(Array(3, 2, 1)) should be(Array(1, 2, 3))
  }

  "Swap" should "swap elements 1,2,3 to 1,3,2" in {
    Main.swap(Array(1, 2, 3), 1, 2) should be(Array(1, 3, 2))
  }

  "Insertion sort" should "sort array 5,2,4,6,1,3" in {
    Main.insertionSort(Array(5, 2, 4, 6, 1, 3)) should be(Array(1, 2, 3, 4, 5, 6))
    Main.insertionSort(Array(5, 2, 4, 6, 1, 3, 0)) should be(Array(0, 1, 2, 3, 4, 5, 6))
  }

  "Merge sort" should "sort array 5,2,4,6,1,3" in {
    Main.mergeSort(Array(5, 2, 4, 6, 1, 3)) should be(Array(1, 2, 3, 4, 5, 6))
    Main.mergeSort(Array(5, 2, 4, 6, 1, 3, 0)) should be(Array(0, 1, 2, 3, 4, 5, 6))
  }

  "Merge method" should "merge two arrays" in {
    val ar = Array(1, 2, 1, 3, 2, 3, 7, 8, 9)
    val p  = 2
    val q  = 3
    val r  = 5
    Main.merge(ar, p, q, r) should be(Array(1, 2, 1, 2, 3, 3, 7, 8, 9))
  }

  "Merge method for minimum elements" should "merge two arrays" in {
    val ar = Array(42, 4)
    val p  = 0
    val q  = 0
    val r  = 1
    Main.merge(ar, p, q, r) should be(Array(4, 42))
  }

  "Bubble sort" should "sort arrays" in {
    Main.bubbleSort(Array(5, 2, 4, 6, 1, 3)) should be(Array(1, 2, 3, 4, 5, 6))
    Main.bubbleSort(Array(5, 2, 4, 6, 1, 3, 0)) should be(Array(0, 1, 2, 3, 4, 5, 6))
  }

  "Max Sum Subarray " should "return" in {
    Main.findMaxSubArray(Array(5)) should be((0, 0, 5))
    Main.findMaxSubArray(Array(5, 1)) should be((0, 1, 6))
    Main.findMaxSubArray(Array(5, 2, 4, 6, 1, 3)) should be((0, 5, 21))
    Main.findMaxSubArray(Array(5, 2, 4, 6, -4, 3)) should be((0, 3, 17))
    Main.findMaxSubArray(Array(-10, 5, 2, 4, 6, -4, 3)) should be((1, 4, 17))
    Main.findMaxSubArray(Array(20, -100, 50)) should be((2, 2, 50))
    Main.findMaxSubArray(Array(20, -16, -12, 3, 4)) should be((0, 0, 20))
  }

  "Max Sum Subarray in linear way" should "return" in {
    Main.findMaxSubArrayLinear(Array(5)) should be((0, 0, 5))
    Main.findMaxSubArrayLinear(Array(5, 1)) should be((0, 1, 6))
    Main.findMaxSubArrayLinear(Array(5, 2, 4, 6, 1, 3)) should be((0, 5, 21))
    Main.findMaxSubArrayLinear(Array(5, 2, 4, 6, -4, 3)) should be((0, 3, 17))
    Main.findMaxSubArrayLinear(Array(-10, 5, 2, 4, 6, -4, 3)) should be((1, 4, 17))
    Main.findMaxSubArrayLinear(Array(20, -100, 50)) should be((2, 2, 50))
    Main.findMaxSubArrayLinear(Array(20, -16, -12, 3, 4)) should be((0, 0, 20))
  }

}
