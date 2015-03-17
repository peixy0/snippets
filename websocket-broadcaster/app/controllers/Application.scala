package controllers

import akka.actor._
import play.api.libs.iteratee._
import play.api.mvc._

import actors.Broadcaster

object Application extends Controller {
  val system = ActorSystem()
  import system._
  
  val broadcaster = system.actorOf(Props(new Broadcaster))
  
  def index = WebSocket.using[String] { request =>
    val (out, channel) = Concurrent.broadcast[String]
    
    broadcaster ! Broadcaster.Channel(channel)
    
    val in = Iteratee.foreach[String] { msg =>
      broadcaster ! Broadcaster.Message(msg)
    } map { _ =>
      broadcaster ! Broadcaster.Done(channel)
    }
    
    (in, out)
  }
}