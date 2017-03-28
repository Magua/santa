package advent

import advent.December1.{ Location, NORTH }
import org.scalatest._

class December1Spec extends FlatSpec with Matchers {

  "North.walk" should "should generate one location for 1 step" in {
    val locations: Seq[Location] = NORTH.walk(Location(0, 0), 1)
    locations should be(Seq(Location(1, 0)))
  }

  it should "should generate two locations for 2 steps" in {
    val locations: Seq[Location] = NORTH.walk(Location(0, 0), 2)
    locations should be(Seq(Location(2, 0), Location(1, 0)))
  }

  it should "should generate last location as head" in {
    val locations: Seq[Location] = NORTH.walk(Location(0, 0), 2)
    locations.head should be(Location(2, 0))
  }

  "Following R2, L3" should "leave you 2 blocks East and 3 blocks North, or 5 blocks away." in {
    val (direction, distance) = December1.move("R2, L3")
    distance should be(5)
  }

  "R2, R2, R2" should "leave you 2 blocks due South of your starting position, which is 2 blocks away." in {
    val (direction, distance) = December1.move("R2, R2, R2")
    distance should be(2)
  }

  "R5, L5, R5, R3" should "leave you 12 blocks away" in {
    val (direction, distance) = December1.move("R5, L5, R5, R3")
    distance should be(12)
  }

  "R1, L4, L5, L5, R2, R2, L1, L1, R2, L3, R4, R3, R2, L4, L2, R5, L1, R5, L5, L2, L3, L1, R1, R4, R5, " +
    "L3, R2, L4, L5, R1, R2, L3, R3, L3, L1, L2, R5, R4, R5, L5, R1, L190, L3, L3, R3, R4, R47, L3, R5, " +
    "R79, R5, R3, R1, L4, L3, L2, R194, L2, R1, L2, L2, R4, L5, L5, R1, R1, L1, L3, L2, R5, L3, L3, R4, " +
    "R1, R5, L4, R3, R1, L1, L2, R4, R1, L2, R4, R4, L5, R3, L5, L3, R1, R1, L3, L1, L1, L3, L4, L1, L2," +
    " R1, L5, L3, R2, L5, L3, R5, R3, L4, L2, R2, R4, R4, L4, R5, L1, L3, R3, R4, R4, L5, R4, R2, L3, R4," +
    " R2, R1, R2, L4, L2, R2, L5, L5, L3, R5, L5, L1, R4, L1, R1, L1, R4, L5, L3, R4, R1, L3, R4, R1, L3," +
    " L1, R1, R2, L4, L2, R1, L5, L4, L5" should "levee you right back at original location" in {

      val (direction, distance) = December1.move("R1, L4, L5, L5, R2, R2, L1, L1, R2, L3, R4, R3, R2, L4, L2, R5, L1, R5, L5, L2, L3, L1, R1, R4, R5, " +
        "L3, R2, L4, L5, R1, R2, L3, R3, L3, L1, L2, R5, R4, R5, L5, R1, L190, L3, L3, R3, R4, R47, L3, R5, " +
        "R79, R5, R3, R1, L4, L3, L2, R194, L2, R1, L2, L2, R4, L5, L5, R1, R1, L1, L3, L2, R5, L3, L3, R4, " +
        "R1, R5, L4, R3, R1, L1, L2, R4, R1, L2, R4, R4, L5, R3, L5, L3, R1, R1, L3, L1, L1, L3, L4, L1, L2," +
        " R1, L5, L3, R2, L5, L3, R5, R3, L4, L2, R2, R4, R4, L4, R5, L1, L3, R3, R4, R4, L5, R4, R2, L3, R4," +
        " R2, R1, R2, L4, L2, R2, L5, L5, L3, R5, L5, L1, R4, L1, R1, L1, R4, L5, L3, R4, R1, L3, R4, R1, L3," +
        " L1, R1, R2, L4, L2, R1, L5, L4, L5")
      distance should be(230)
    }

  "R8, R4, R4, R8" should "the first location you visit twice is 4 blocks away, due East." in {
    val distance = December1.firstLocationVisitedTwice("R8, R4, R4, R8")
    distance should be(4)
  }

  "R1, L4, L5, L5, R2, R2, L1, L1, R2, L3, R4, R3, R2, L4, L2, R5, L1, R5, L5, L2, L3, L1, R1, R4, R5, " +
    "L3, R2, L4, L5, R1, R2, L3, R3, L3, L1, L2, R5, R4, R5, L5, R1, L190, L3, L3, R3, R4, R47, L3, R5, " +
    "R79, R5, R3, R1, L4, L3, L2, R194, L2, R1, L2, L2, R4, L5, L5, R1, R1, L1, L3, L2, R5, L3, L3, R4, " +
    "R1, R5, L4, R3, R1, L1, L2, R4, R1, L2, R4, R4, L5, R3, L5, L3, R1, R1, L3, L1, L1, L3, L4, L1, L2," +
    " R1, L5, L3, R2, L5, L3, R5, R3, L4, L2, R2, R4, R4, L4, R5, L1, L3, R3, R4, R4, L5, R4, R2, L3, R4," +
    " R2, R1, R2, L4, L2, R2, L5, L5, L3, R5, L5, L1, R4, L1, R1, L1, R4, L5, L3, R4, R1, L3, R4, R1, L3," +
    " L1, R1, R2, L4, L2, R1, L5, L4, L5" should "leave you 154 blocks away" in {
      val distance = December1.firstLocationVisitedTwice("R1, L4, L5, L5, R2, R2, L1, L1, R2, L3, R4, R3, R2, L4, L2, R5, L1, R5, L5, L2, L3, L1, R1, R4, R5, " +
        "L3, R2, L4, L5, R1, R2, L3, R3, L3, L1, L2, R5, R4, R5, L5, R1, L190, L3, L3, R3, R4, R47, L3, R5, " +
        "R79, R5, R3, R1, L4, L3, L2, R194, L2, R1, L2, L2, R4, L5, L5, R1, R1, L1, L3, L2, R5, L3, L3, R4, " +
        "R1, R5, L4, R3, R1, L1, L2, R4, R1, L2, R4, R4, L5, R3, L5, L3, R1, R1, L3, L1, L1, L3, L4, L1, L2," +
        " R1, L5, L3, R2, L5, L3, R5, R3, L4, L2, R2, R4, R4, L4, R5, L1, L3, R3, R4, R4, L5, R4, R2, L3, R4," +
        " R2, R1, R2, L4, L2, R2, L5, L5, L3, R5, L5, L1, R4, L1, R1, L1, R4, L5, L3, R4, R1, L3, R4, R1, L3," +
        " L1, R1, R2, L4, L2, R1, L5, L4, L5")
      distance should be(154)
    }
}