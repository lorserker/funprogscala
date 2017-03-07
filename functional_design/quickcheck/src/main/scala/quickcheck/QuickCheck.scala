package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
  	v <- arbitrary[Int]
  	h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
  	val h = insert(a, empty)
  	findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
  	val h = insert(a, insert(b, empty))
  	findMin(h) == (a min b)
  }

  property("empty insert del is empty") = forAll { (a: Int) => 
  	val h = deleteMin(insert(a, empty))
  	isEmpty(h)
  }

  property("minimum of melded") = forAll { (h1: H, h2: H) =>
  	val m1 = findMin(h1)
  	val m2 = findMin(h2)
  	val hMelded = meld(h1, h2)
  	findMin(hMelded) == (m1 min m2)
  }

  property("findMin returns increasing vals") = forAll { (h: H) =>
  	val values = minList(h)
  	isSorted(values)
  }

  property("meld two empty heaps") = {
  	val h = meld(empty, empty)
  	isEmpty(h)
  }

  property("meld with empty") = forAll { (h: H) =>
  	val minsBefore = minList(h)
  	val hMelded = meld(h, empty)
  	val minsAfter = minList(hMelded)

  	minsBefore.length == minsAfter.length &&
  	(minsBefore zip minsAfter).forall{ case (l, r) => l == r }
  }

  property("melding should result in heap") = forAll { (h1: H, h2: H) =>
  	val h = meld(h1, h2)
  	isSorted(minList(h))
  }

  property("after delete should still be heap") = forAll { (h: H) =>
  	isSorted(minList(deleteMin(h)))
  }

  property("after insert should still be heap") = forAll { (h: H) =>
  	val x = arbitrary[Int].sample.get
  	isSorted(minList(insert(x, h)))
  }

  property("deleteMin deletes smallest") = forAll { (h: H) =>
  	val v1 = arbitrary[Int].sample.get
  	val v2 = arbitrary[Int].sample.get
  	val h1 = insert(v1, insert(v2, h))
  	val m = findMin(h1)
  	val hDel = deleteMin(h1)
  	findMin(hDel) >= m
  }

  property("delete from one element heap") = forAll { (a: Int) =>
  	val h = insert(a, empty)
  	isEmpty(deleteMin(h))
  }

  property("heap from list") = forAll { (lst: List[Int]) =>
  	val h = lst.foldLeft(empty)((acc, x) => insert(x, acc))
  	lst.sorted == minList(h)
  }


  private def minList(h: H): List[Int] = {
  	if (isEmpty(h))
  	  Nil
  	else
  	  findMin(h) :: minList(deleteMin(h))
  }

  private def isSorted(vals: List[Int]): Boolean = {
  	vals match {
  		case Nil => true
  		case v :: Nil => true
  		case v1 :: v2 :: rest => if (v1 > v2) false else isSorted(v2 :: rest)
  	}
  }

}
