/**
  * Helper class used to store the result of a join of two sequences L and R
  * @param l The elements only contained in L
  * @param c The joined element pairs (joined by equality of some key)
  * @param r The elements only contained in R
  * @tparam L The type of the elements in the left sequence
  * @tparam R The type of the elements in the right sequence
  */
case class LCR[+L,+R](l:Seq[L], c:Seq[(L,R)], r:Seq[R])

/**
  * Aufgabe 1:
  *
  * Join two sequences by a common key. Return a sequence of joined tuples and the open left and right elements.
  *
  * @param lefts First sequence to join
  * @param rights Second sequence to join
  * @param lid Key function determining a key from L elements
  * @param rid Key function determining a key from L elements
  * @tparam L The element type of the left sequence
  * @tparam R The element type of the right sequence
  * @tparam K The join key type
  * @return Data structure containing the joined tuples and the left and right open elements
  */
def join[L,R,K:Ordering](lefts:Seq[L], rights:Seq[R], lid:L=>K, rid:R=>K) : LCR[L,R] = ???


import scala.annotation.tailrec

def zipMatching[A, B, C](a: List[A], b: List[B], projA: A => C, projB: B => C)(implicit ord: Ordering[C]): List[(A, B)] = {
  import ord._ // Get access to `>`

  @tailrec
  def zipSortedMatching(aSorted: List[A], bSorted: List[B], accum: List[(A, B)]): List[(A, B)] = {

    (aSorted, bSorted) match {
      case (aHead +: aRest, bHead +: bRest) =>
        val compA = projA(aHead)
        val compB = projB(bHead)

        if (compA == compB)
          zipSortedMatching(aRest, bRest, accum :+ (aHead, bHead))
        else if (compA > compB)
          zipSortedMatching(aHead +: aRest, bRest, accum)
        else
          zipSortedMatching(aRest, bHead +: bRest, accum)
      case _ =>
        accum
    }
  }

  zipSortedMatching(a.sortBy(projA), b.sortBy(projB), Nil)
}


case class AA(serial: Int)
case class BB(serial: Int)

val l1 = List(0, 1, 2, 3, 4, 5).map(AA(_))
val l2 = List(1, 3, 0, 2, 4).map(BB(_))

def projAA(a: AA) = a.serial
def projBB(b: BB) = b.serial

val pairs = zipMatching(l1, l2, projAA, projBB)