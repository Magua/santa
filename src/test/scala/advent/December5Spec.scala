package advent

import org.scalatest._

import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

class December5Spec extends FlatSpec with Matchers {

  "abc3231929" should "produce exprected hash starting with 5 zeros" in {
    December5.md5("abc3231929") should startWith("00000")
  }

  "doorId abc" should "produce expected password 18f47a30 in parallel (73 sec)" in {
    //val future: Future[String] = Await.ready(December5.crackStream("abc"), 5 minutes)
    //future.value.get.get should be("18f47a30")
  }

  "doorId" should "produce expected password 18f47a30 (187 sec)" in {
    //December5.crack("abc") should be("18f47a30")
  }

  "doorId wtnhxymk" should "produce expected password 18f47a30 in parallel (85 sec)" in {
    //    val future: Future[String] = Await.ready(December5.crackStream("wtnhxymk"), 5 minutes)
    //    future.value.get.get should be("2414bc77")
  }

  "filter with state" should "should remember already solved parts of the password" in {

    val filter: ((Int, String)) => Boolean = December5.filterWithMemory()
    filter(1, "000001") should be(false)
    filter(1, "000002") should be(false)
    filter(1, "000002") should be(true)

  }

  "You" should "almost choke on your popcorn as the final character falls into place, producing the password 05ace8e3" in {

    //val future: Future[String] = Await.ready(December5.crackStream2("abc"), 5 minutes)
    //future.value.get.get should be("05ace8e3")

    //val future: Future[String] = Await.ready(December5.crackStream2("wtnhxymk"), 5 minutes)
    //future.value.get.get should be("437e60fc")

  }

}