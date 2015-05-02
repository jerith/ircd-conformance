package ircd_conformance.test

import akka.actor.{ Actor, ActorLogging, ActorRef, ActorSystem, Props, Terminated }
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import java.net.InetSocketAddress
import scala.concurrent.Promise
import akka.event.LoggingReceive

object FakeServer {
  def props(endpoint: InetSocketAddress, handler: ActorRef): Props =
    Props(new FakeServer(endpoint, handler))
}

class FakeServer(endpoint: InetSocketAddress, val handler: ActorRef) extends Actor {
  import Tcp._
  import context.system
  import context.dispatcher

  IO(Tcp) ! Bind(self, endpoint)

  val addrPromise = Promise[InetSocketAddress]()
  var clients = Map[InetSocketAddress, ActorRef]()

  override def receive: Receive = LoggingReceive {
    case b @ Bound(localAddress) =>
      addrPromise.success(localAddress)
    case "addr" =>
      val asker = sender
      addrPromise.future.onSuccess {
        case addr =>
          asker ! addr
      }
    case Tcp.Connected(remote, _) =>
      val client = context.actorOf(
        ConnectionHandler.props(remote, sender, handler),
        s"c:${remote.getPort}")
      context.watch(client)
      clients += remote -> client
      sender ! Tcp.Register(client)
      handler ! ("connected", remote)
    case Terminated(client) =>
      val remote = clients.find(_._2 == client).map(_._1)
      remote.map { remote =>
        clients -= remote
        handler ! ("disconnected", remote)
      }
    case (remote: InetSocketAddress, data) =>
      clients.get(remote).map(_ ! data)
  }
}

object ConnectionHandler {
  def props(remote: InetSocketAddress, conn: ActorRef, handler: ActorRef): Props =
    Props(new ConnectionHandler(remote, conn, handler))
}

class ConnectionHandler(remote: InetSocketAddress, conn: ActorRef, handler: ActorRef) extends Actor {
  import Tcp._

  context.watch(conn)

  def receive: Receive = LoggingReceive {
    case "close" =>
      context.stop(self)
    case Received(data) =>
      handler ! (remote, data)
    case _: Tcp.ConnectionClosed =>
      context.stop(self)
    case Terminated(`conn`) =>
      context.stop(self)
    case data: ByteString =>
      conn ! Write(data)
  }
}
