package actors

import scala.collection.mutable
import akka.actor._
import play.api.libs.iteratee._

class Broadcaster extends Actor {
  import Broadcaster._
  
  val channels = mutable.Set[Concurrent.Channel[String]]()
  
  def receive = {
    case Channel(ch) =>
      channels += ch
    case Done(ch) =>
      channels -= ch
    case Message(msg) =>
      channels foreach (_ push msg)
  }
}

object Broadcaster {
  case class Channel(ch: Concurrent.Channel[String])
  case class Done(ch: Concurrent.Channel[String])
  case class Message(msg: String)
}