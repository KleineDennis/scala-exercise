case class LCR[+L,+R](l:Seq[L], c:Seq[(L,R)], r:Seq[R])

def join[L,R,K:Ordering](lefts:Seq[L], rights:Seq[R], lid:L=>K, rid:R=>K): LCR[L,R] = {
//  val l = lefts.map(t => lid(t)->t).toMap.toList
//  val r = rights.map(t => rid(t)->t).toMap

  val l = lefts.groupBy(lid(_)).toList
  val r = rights.groupBy(rid(_))
  val pairs = l.flatMap{ case (k,as) => r.get(k).toList.flatMap(as.zip(_)) }

  val diffL = lefts.diff(pairs.map(_._1))
  val diffR = rights.diff(pairs.map(_._2))

  LCR(diffL,pairs,diffR)
}


case class L(id: Int)
case class R(id: Int)

val lefts = List(5,1,2,4,3).map(L(_))
val rights = List(1, 3, 0, 2, 4).map(R(_))

def lid(l: L) = l.id
def rid(r: R) = r.id

//val pairs = join(lefts, rights, lid, rid)
val l1 = lefts.groupBy(lid(_)).toList
val r1 = rights.groupBy(rid(_))
val l = lefts.map(t => lid(t)->t).toMap
val r = rights.map(t => rid(t)->t).toMap

val pairs1 = l1.flatMap{ case (k,as) => r1.get(k).toList }

val pairs = l.flatMap{ case (k,v) => r.get(k).toList}


