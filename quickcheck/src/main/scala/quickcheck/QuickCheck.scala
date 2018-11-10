package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(genHeap, const(empty))
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert2") = forAll {(a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    val min = findMin(h2)
    min == Math.min(a, b)
  }

  property("emptyOps") = forAll { a: Int =>
    val h = insert(a, empty)
    val h1 = deleteMin(h)
    isEmpty(h1)
  }

  property("sorted") = forAll { h: H =>
    def isSorted(h: H, min: Int): Boolean = {
      if (isEmpty(h)) true
      else if (min > findMin(h)) false
      else isSorted(deleteMin(h), findMin(h))
    }
    isSorted(h, findMin(h))
  }

  property("melding") = forAll {(h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val newHeap = meld(h1, h2)
    findMin(newHeap) == Math.min(min1, min2)
  }


  // Take two arbitrary heaps, meld together.
  // Then remove min from 1 and insert into 2, meld the results.
  // Compare two melds by comparing sequences of ranks.
  property("meldMinMove") = forAll { (h1: H, h2: H) =>
    def remMin(ts: H, as: List[Int]): List[Int] = {
      if (isEmpty(ts)) as
      else findMin(ts) :: remMin(deleteMin(ts), as)
    }
    val meld1 = meld(h1, h2)
    val min1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(min1, h2))
    val xs1 = remMin(meld1, Nil)
    val xs2 = remMin(meld2, Nil)
    xs1 == xs2
  }

}
