package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  lazy val genMap: Gen[Map[Int, Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int, Int]), genMap)
  } yield m.updated(k, v)

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
//    if isEmpty(h) // for generators, don't generate empty heap
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("after inserting, heap is not empty") = forAll{
    (h:H) => isEmpty(insert(0, h))==false
  }

  property("inserting min value to heap will put it on top of heap") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("inserting element to empty heap will put it on top") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  //hint1. If you insert any two elements into an empty heap,
  // finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("insert any two elements into an empty heap") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == Math.min(a, b) && findMin(deleteMin(h))==Math.max(a,b)
  }

  //hint2. If you insert an element into an empty heap, then delete the minimum,
  // the resulting heap should be empty.
  property("insert an element into an empty heap, then delete the minimum") = forAll { a: Int =>
    val h = insert(a, empty)
    val h2 = deleteMin(h)
    isEmpty(h2)
  }

  //hint3. Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  // (Hint: recursion and helper functions are your friends.)
  def is_sorted(h: H): Boolean ={
    def is_sorted_tailrec(h: H, last: Int): Boolean =
    if (isEmpty(h)) true
    else {
      val m = findMin(h)
      m>=last && is_sorted_tailrec(deleteMin(h), m)
    }
    is_sorted_tailrec(h, Integer.MIN_VALUE)
  }



  property("Continually finding and deleting minima") = forAll { h: H =>
    is_sorted(h)
  }

  //hint4. Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("meld (non empty) h with empty heap") = forAll { (h: H) =>
    if (isEmpty(h)) true
    else findMin(meld(h, empty)) == findMin(h)
  }

  property("melding of any two heaps") = forAll{(h1:H, h2:H) =>
    val melded = meld(h1, h2)
    if(isEmpty(h1) || isEmpty(h2) || isEmpty(melded)) true
    else findMin(melded) == Math.min(findMin(h1), findMin(h2))
  }

  def size(h:H): Int = {
    @tailrec
    def size_tailrec(h:H, acc:Int):Int =
      if(isEmpty(h)) acc
      else size_tailrec(deleteMin(h), acc+1)
    size_tailrec(h, 0)
  }

  property("melding of any two heaps, size match") = forAll{(h1:H, h2:H) =>
    size(meld(h1, h2)) == size(h1)+size(h2)
  }

  def elements(h:H): List[Int]= {
    @tailrec
    def elements_tailrec(h:H, acc:List[Int]):List[Int] =
      if(isEmpty(h)) acc
      else elements_tailrec(deleteMin(h), findMin(h)::acc)
    elements_tailrec(h, Nil)
  }

  property("melding of any two heaps, all elements match") = forAll{(h1:H, h2:H) =>
    elements(meld(h1, h2)).sorted == (elements(h1)++elements(h2)).sorted
  }


  property("insert 3 times and del 3 times") = forAll{ a:Int =>
    val h1 = insert(a, empty)
    val h2 = insert(a, h1)
    val h3 = insert(a, h2)
    val eh = deleteMin( deleteMin(deleteMin(h3)) )
    isEmpty(eh)

  }


}
