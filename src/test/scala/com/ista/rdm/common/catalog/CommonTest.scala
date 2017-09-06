package com.ista.rdm.common.catalog

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import com.ista.rdm.common.catalog.Common._

/**
  * Created by stoyema on 09.08.2017.
  */
class CommonTest extends FlatSpec {

  // Test data structures
  case class TSA(id:String, load:Int)
  def tsa_id : TSA => String = _.id
  case class TSB(id:String, load:String)
  def tsb_id : TSB => String = _.id

  val (a1,a2,a3,a4,a5) = (TSA("a",8),TSA("b",3),TSA("c",16),TSA("d",986),TSA("e",1))
  val (b1,b2,b3,b4,b5) = (TSB("a","XYZ"),TSB("b","KRK"),TSB("c",""),TSB("d","ddfh"),TSB("e","XYZ"))

  "Joining two empty sequences" should "result in an empty result structure" in {
    Common.join[Int](Seq(), Seq()) shouldBe LCR(Nil, Nil, Nil)
  }

  "Joining an empty sequence with a nonempty sequence" should "result in the nonempty sequence in the open results" in {
    Common.join(Seq(1, 2, 3), Seq()) shouldBe LCR(Seq(1, 2, 3), Nil, Nil)
    Common.join(Seq(), Seq(1, 2, 3)) shouldBe LCR(Nil, Nil, Seq(1, 2, 3))
  }

  "Joining two disjunct nonempty sequences" should "result in the sequences in the open results" in {
    Common.join(Seq(1, 2, 3), Seq(4, 5, 6)) shouldBe LCR(Seq(1, 2, 3), Nil, Seq(4, 5, 6))
    Common.join(Seq(4, 5, 6), Seq(1, 2, 3)) shouldBe LCR(Seq(4, 5, 6), Nil, Seq(1, 2, 3))
    Common.join(Seq(1, 3, 6), Seq(2, 4, 7)) shouldBe LCR(Seq(1, 3, 6), Nil, Seq(2, 4, 7))
  }

  "Joining two equal nonempty sequences" should "result in the sequences in the joined results" in {
    Common.join(Seq(1, 2, 3), Seq(1, 2, 3)) shouldBe LCR(Nil, Seq((1,1),(2,2),(3,3)), Nil)
    Common.join(Seq(a1,a2,a3), Seq(a1,a2,a3), tsa_id) shouldBe LCR(Nil, Seq((a1,a1),(a2,a2),(a3,a3)), Nil)
    Common.join(Seq(a1,a2,a3), Seq(b1,b2,b3), tsa_id, tsb_id) shouldBe LCR(Nil, Seq((a1,b1),(a2,b2),(a3,b3)), Nil)
  }

  "Joining two intersecting nonempty sequences" should "result in the splitting of open and joined results" in {
    Common.join(Seq(1, 3, 5), Seq(1, 2, 3)) shouldBe LCR(Seq(5), Seq((1,1),(3,3)), Seq(2))
    Common.join(Seq(a1,a3,a5), Seq(a1,a2,a3), tsa_id) shouldBe LCR(Seq(a5), Seq((a1,a1),(a3,a3)), Seq(a2))
    Common.join(Seq(a1,a3,a5), Seq(b1,b2,b3), tsa_id, tsb_id) shouldBe LCR(Seq(a5), Seq((a1,b1),(a3,b3)), Seq(b2))
  }

  "Joining two equal nonempty sequences of custom data types with implicit ordering" should "result in the sequences in the joined results" in {
    // test joining with implicit ordering
    implicit val TSA_Ord : Ordering[TSA] = new Ordering[TSA] {
      override def compare(x: TSA, y: TSA): Int = Ordering[String].compare(x.id,y.id)
    }
    Common.join(Seq(a1,a2,a3), Seq(a1,a2,a3)) shouldBe LCR(Nil, Seq((a1,a1),(a2,a2),(a3,a3)), Nil)
  }

  "mapFirst" should "work" in {
    mapFirst[Int,Int](Seq(1, 2, 3, 4), _*2, _==3) shouldBe None
    mapFirst[Int,Int](Seq(1, 2, 3, 4), _*2, _==6) shouldBe Some(6)
    mapFirst[Int,(Int,Int)](Seq(1, 2, 3, 4), x=>(x,2*x), _._2==6) shouldBe Some((3,6))
  }

}
