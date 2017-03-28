package advent

import java.security.MessageDigest.getInstance

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{ Sink, Source }

import scala.concurrent.{ ExecutionContext, Future }

object December5 {
  type MappingStrategy = (Int, String) => Option[(Int, Char)]
  val double = (i: Int) => {
    i * 2
  }
  implicit val as = ActorSystem("cracker")
  implicit val ec = as.dispatcher
  implicit val materializer = ActorMaterializer()

  def crack(doorId: String): String = {
    (0 until Int.MaxValue)
      .view
      .map(n => md5Sync((n, doorId)))
      .filter(startsWith00000)
      .map(_._2.drop(5).head)
      .take(8)
      .foldLeft("") {
        _ + _
      }
  }

  def crackStream(doorId: String): Future[String] = crackStream(doorId, startsWith00000)

  def crackStream(doorId: String, filter: ((Int, String)) => Boolean): Future[String] = {
    Source(0 until Int.MaxValue)
      .mapAsync(7)(n => md5Async(n, doorId))
      .filter(filter)
      .map(_._2.drop(5).head)
      .take(8)
      .fold("")(_ + _)
      .runWith(Sink.head)
  }

  def crackStream2(doorId: String): Future[String] = {
    val filter = filterWithMemory()
    Source(0 until Int.MaxValue)
      .mapAsync(7)(n => md5Async(n, doorId))
      .filterNot(filter)
      .map {
        case (i: Int, s: String) =>
          val trimmed = s.drop(5)
          (trimmed.take(1), trimmed.slice(1, 2))
      }
      .take(8)
      .fold(Array("_", "_", "_", "_", "_", "_", "_", "_")) {
        case (arr, (position, value)) =>
          arr(position.toInt) = value
          println(arr.mkString(""))
          arr
      }
      .map(_.mkString(""))
      .runWith(Sink.head)
  }

  private[advent] def md5Async(t: (Int, String))(implicit ec: ExecutionContext) =
    Future(md5Sync(t))

  private[advent] def md5Sync(t: (Int, String)) =
    (t._1, md5(t._2 + t._1))

  private[advent] def md5(s: String): String = getInstance("MD5")
    .digest(s.getBytes)
    .map(0xFF & _)
    .map("%02x".format(_))
    .foldLeft("")(_ + _)

  private[advent] def filterWithMemory(): (((Int, String)) => Boolean) = {
    var usedPositions = List[String]()
    (t: (Int, String)) => {
      val (i, s) = t
      val position = s.slice(5, 6)
      if (startsWith00000(t) &&
        position >= "0" && "7" >= position &&
        !usedPositions.contains(position)) {
        usedPositions = position :: usedPositions
        false
      } else {
        true
      }
    }
  }

  private[advent] def startsWith00000(t: (Int, String)) = {
    val (i, s) = t

    if (s.startsWith("00000")) {
      //println(s"n($i) -> hash($s)")
      true
    } else {
      false
    }
  }
}
