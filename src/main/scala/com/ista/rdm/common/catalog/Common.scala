package com.ista.rdm.common.catalog

import scala.annotation.tailrec

/**
  * Generic functions for package ServiceCatalog
  */
object Common {

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
  def join[L,R,K:Ordering](lefts:Seq[L], rights:Seq[R], lid:L=>K, rid:R=>K) : LCR[L,R] = {
//    val pairs =
//      for {
//        l <- lefts
//        r <- rights
//        if lid(l) == rid(r)
//      } yield (l,r)

    val l1 = lefts.map(t => lid(t)->t).toMap.toList
    val r1 = rights.map(t => rid(t)->t).toMap

    val l = lefts.groupBy(lid(_)).toList
    val r = rights.groupBy(rid(_))
    val pairs = l.flatMap{ case (k,as) => r.get(k).toList.flatMap(as.zip(_)) }

    val diffL = lefts.diff(pairs.map(_._1))
    val diffR = rights.diff(pairs.map(_._2))

    LCR(diffL,pairs,diffR)
  }


  /**
    * Convenience function when joining sequences of same type
    */
  def join[T,K:Ordering](lefts:Seq[T], rights:Seq[T], idf:T=>K) : LCR[T,T] = join[T,T,K](lefts, rights, idf, idf)

  /**
    * Convenience function when joining sequence of same type where the type itself is also the comparison key
    */
  def id[T](t:T) : T = t
  def join[T:Ordering](lefts:Seq[T], rights:Seq[T]) : LCR[T,T] = join[T,T,T](lefts, rights, id, id)

  /**
    * Aufgabe 2:
    *
    * Map on a sequence with a termination condition. If the condition is true, return the element identified.
    *
    * @param s The sequence
    * @param f The mapping function
    * @param c The termination condition
    * @tparam T The base type of the sequence
    * @tparam U The target type of the map function
    * @return The first element satisfying the termination condition or None
    */
  // @tailrec
//  def mapFirst[T,U](s:Seq[T], f:T=>U, c:U=>Boolean) : Option[U] = {
//    if (s == Nil) None
//    else if (c(f(s.head))) Some(f(s.head))
//    else mapFirst(s.tail, f, c)
//  }

  def mapFirst[T, U](s: Seq[T], f:T=>U, c:U=>Boolean): Option[U] = {
    @tailrec
    def helper[T, U](s: Seq[T], f:T=>U, c:U=>Boolean): Option[U] = {
      if (s == Nil) None
      else if (c(f(s.head))) Some(f(s.head))
      else helper(s.tail, f, c)
    }
    helper(s,f,c)
  }

}
