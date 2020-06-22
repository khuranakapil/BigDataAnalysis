package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import java.util.Random


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  def getlist(h: QuickCheckHeap.this.H): List[A] = {
    if (isEmpty(h)) List() else {
      val x = findMin(h)
      List(x) ::: getlist(deleteMin(h))}
  }

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      valu <- arbitrary[Int]
      m <- oneOf(const(empty), genHeap)
    } yield insert(valu, m)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll{ (a:Int) =>
    val newheap = insert(a, empty)
    findMin(newheap) == a
  }

  property("min2") = forAll{(a:Int, b:Int) =>
    val newheap = insert(a, empty)
    val newheap1 =insert(b, newheap)
    if (a < b) findMin(newheap1) == a else findMin(newheap1) == b
  }

  property("delete1") = forAll{(a:Int) =>
    val newheap = insert(a, empty)
    val newheap1 = deleteMin(newheap)
    isEmpty(newheap1)
  }

  property("delete2") = forAll{(a:Int) =>
    val newheap = insert(a, empty)
    val newheap2 = insert(-5, newheap)
    val newheap3 = insert(-15, newheap2)
    val newheap4 = insert(-15, newheap3)
    val newheap5 = insert(50, newheap4)
    val newheap1 = deleteMin(newheap5)
    findMin(newheap1) == -15
  }

  property("meld1") = forAll{(h1:H, h2:H) =>
    val h3 = meld(h1, h2)
    if (isEmpty(h1) && isEmpty(h2)) isEmpty(h3) else if(isEmpty(h1)) findMin(h3) == findMin(h2) else if(isEmpty(h2)) findMin(h3) == findMin(h1) else
    findMin(h3) == findMin(h1) || findMin(h3) == findMin(h2)
  }

  property("sort1") = forAll { (h1: H, h2: H, h3: H, h4: H) =>
    val h5 = meld(h1, h2)
    val h6 = meld(h5, h3)
    val h7 = meld(h6, h4)
    val l: List[A] = getlist(h7)
    //println(l.toString())
    //println(l.sorted.toString())
    if (l.isEmpty) true else l == l.sorted
  }
  def getrandlist(value: Int):List[A] = {
    val r = scala.util.Random
    val a = r.nextInt
    if (value == 0) List() else List(r.nextInt) ::: getrandlist(value - 1)
  }
  def getheap(value1: List[A]):H = {
    if (value1.isEmpty) empty else meld(insert(value1.head, empty), getheap(value1.tail))
  }
  property("sort2") = forAll{(a:Int) =>
    val l1:List[A] = getrandlist(10)
    val l = getlist(getheap(l1))
    if (l.isEmpty) true else l == l.sorted
  }
  property("sort3") = forAll{(a:Int) =>
    val l1:H = insert(20, empty)
    val l2 = insert(-4, l1)
    val l3 = insert(5, l2)
    val l4 = insert(0, l3)
    val l5 = insert(-10, l4)

    val l = getlist(l5)
    if (l.isEmpty) true else l == l.sorted
  }
}
