package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("the minimum of a melded heap should be one of the minimums of its parts") = 
    forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) && !isEmpty(h2)) ==> {
      val min1 = findMin(h1)
  	  val min2 = findMin(h2)
  	  val melded = meld(h1, h2)
  	  val minMelded = findMin(melded)
      minMelded == min1 || minMelded == min2
    }
  }
  
  property("minimums should be sorted") = forAll { h: H =>
    def isSortedAscending(heap: H, previousMin: Int): Boolean = {
      if (isEmpty(heap))
        true
      else {
        val min = findMin(heap)
        val tailHeap = deleteMin(heap)
        if (min < previousMin)
          false
        else
          isSortedAscending(tailHeap, min)
      }
    }
    isSortedAscending(h, Int.MinValue)
  }
  
  property("the minimum of a heap sized two should be the smaller one of what was inserted") = 
    forAll { (a: Int, b: Int) =>
      val heapWithTwo = insert(a, insert(b, empty))
      findMin(heapWithTwo) == Math.min(a, b)
  }
  
  property("deleteMin from of a heap sized two should retain the larger one of what was inserted") = 
    forAll { (a: Int, b: Int) =>
      val heapWithTwo = insert(a, insert(b, empty))
      val afterDeleteMin = deleteMin(heapWithTwo)
      findMin(afterDeleteMin) == Math.max(a, b)
  }
  
  property("two deleteMin from of a heap sized three should retain the largest one of what was inserted") = 
    forAll { (a: Int, b: Int, c: Int) =>
      val heapWithThree = insert(a, insert(b, insert(c, empty)))
      val afterDeleteFirstMin = deleteMin(heapWithThree)
      val afterDeleteSecondMin = deleteMin(afterDeleteFirstMin)
      findMin(afterDeleteSecondMin) == Math.max(a, Math.max(b, c))
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h1 <- oneOf(value(empty), genHeap)
    h2 <- oneOf(value(h1), value(insert(x, h1)))
  } yield h2
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
