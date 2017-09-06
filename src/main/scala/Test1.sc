

//def f(arr:List[Int]):List[Int] =
//  arr.reverse.head ::



//val arr = List(19,22,3,28,26,17,18,4,28,0)

//var result = List[Int]()
//var theses = list
//while (!theses.isEmpty) {
//  result = theses.head :: result
//  theses = theses.tail
//}
//result


//arr.foldLeft( List[Int]() ) ((acc,v) => v::acc)

//val arr = List(-1, 3,2,4,6,5,7,8,0,1)
//arr filter(_ % 2 != 0) sum

val arr = List(2,5,1,4,3,7,8,6,0,9)

arr.map(x => 1).sum
arr.foldLeft(0)( {case(acc,i) => acc+1} )
arr.foldLeft(0)( (acc,i) => acc+1 )
arr.foldLeft(0)((acc,v) => acc+1)

