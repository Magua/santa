package advent

import scala.annotation.tailrec

object December1 {

  def move(directions: String): (Direction, Int) = {
    val (direction, steps, _) = moveWithLocations(directions)
    (direction, steps)
  }

  def moveWithLocations(directions: String): (Direction, Int, Seq[Location]) = {

    val movesAndSteps = directionsToMovesAndSteps(directions)

    val (facing, location, locations) = movesAndSteps.foldLeft((NORTH: Direction, Location(0, 0), Seq[Location]())) { (curr, step) =>
      val (facing: Direction, location, locations) = curr
      val (move, steps) = step
      val newFacing = facing.face(move)
      val newLocations: Seq[Location] = newFacing.walk(location, steps)
      (newFacing, newLocations.head, locations ++ newLocations)
    }

    (facing, location.toSteps, locations)
  }

  def directionsToMovesAndSteps(directions: String): Array[(Move, Int)] = {
    directions
      .filterNot((x: Char) => x.isWhitespace)
      .split(",")
      .map((d: String) => {
        (d: Seq[Char]) match {
          case 'R' +: n => (Right, n.toString.toInt)
          case 'L' +: n => (Left, n.toString.toInt)
        }
      })
  }

  @tailrec
  def findDuplicateLocation(visitedLocations: Seq[Location]): Option[Location] = visitedLocations match {
    case Nil                                 => None
    case head :: tail if tail.contains(head) => Some(head)
    case head :: tail                        => findDuplicateLocation(tail)

  }

  def firstLocationVisitedTwice(directions: String): Int = {
    val (_, _, locations) = moveWithLocations(directions)
    findDuplicateLocation(locations) match {
      case Some(l) => l.toSteps
      case None    => -1
    }
  }

  sealed trait Direction {
    def face(move: Move): Direction

    def walk(location: Location, steps: Int): Seq[Location]
  }

  sealed trait Move

  case class Location(ns: Int, ew: Int) {
    lazy val toSteps = Math.abs(ns) + Math.abs(ew)
  }

  case object NORTH extends Direction {
    override def face(move: Move): Direction = move match {
      case Left  => WEST
      case Right => EAST
    }

    override def walk(location: Location, steps: Int): Seq[Location] = for (i <- steps to 1 by -1) yield location.copy(ns = location.ns + i)
  }

  case object EAST extends Direction {
    override def face(move: Move): Direction = move match {
      case Left  => NORTH
      case Right => SOUTH
    }

    override def walk(location: Location, steps: Int) = for (i <- steps to 1 by -1) yield location.copy(ew = location.ew + i)
  }

  case object SOUTH extends Direction {
    override def face(move: Move): Direction = move match {
      case Left  => EAST
      case Right => WEST
    }

    override def walk(location: Location, steps: Int) = for (i <- steps to 1 by -1) yield location.copy(ns = location.ns - i)
  }

  case object WEST extends Direction {
    override def face(move: Move): Direction = move match {
      case Left  => SOUTH
      case Right => NORTH
    }

    override def walk(location: Location, steps: Int) = for (i <- steps to 1 by -1) yield location.copy(ew = location.ew - i)
  }

  case object Right extends Move

  case object Left extends Move

}
