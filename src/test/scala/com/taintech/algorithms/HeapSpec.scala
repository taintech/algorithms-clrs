package com.taintech.algorithms

import org.scalatest.{ Matchers, WordSpec }

class HeapSpec extends WordSpec with Matchers {

  import Heap._

  "Heap" should {
    "return max heapify" in {
      val ar = Array(16, 4, 10, 14, 7, 9, 3, 2, 8, 1)
      maxHeapify(ar, 1, ar.length) should be(Array(16, 14, 10, 8, 7, 9, 3, 2, 4, 1))
    }
    "return build max heap" in {
      val ar = Array(16, 4, 10, 14, 7, 9, 3, 2, 8, 1)
      buildMaxHeap(ar) should be(Array(16, 14, 10, 8, 7, 9, 3, 2, 4, 1))
    }
    "return sort array" in {
      heapSort(Array(16, 4, 10, 14, 7, 9, 3, 2, 8, 1)) should be(Array(1, 2, 3, 4, 7, 8, 9, 10, 14, 16))
      heapSort(Array(5, 2, 4, 6, 1, 3)) should be(Array(1, 2, 3, 4, 5, 6))
      heapSort(Array(5, 2, 4, 6, 1, 3, 0)) should be(Array(0, 1, 2, 3, 4, 5, 6))
      heapSort(Array(5, 2, 4, 0, 1, 3, 100)) should be(Array(0, 1, 2, 3, 4, 5, 100))
    }
  }

}
