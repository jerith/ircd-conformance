package ircd_conformance.test

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Terminated}
import akka.event.LoggingReceive
import akka.io.{IO, Tcp}
import akka.util.ByteString
import java.net.InetSocketAddress
import scala.concurrent.{Future, Promise}

object FakeServer {
  def props(endpoint: InetSocketAddress, handler: ActorRef): Props =
    Props(new FakeServer(endpoint, handler))
}

class FakeServer(endpoint: InetSocketAddress, val handler: ActorRef) extends Actor {
  import Tcp._
  import context.system
  import context.dispatcher

  IO(Tcp) ! Bind(self, endpoint)
  var listener: ActorRef = _

  val addrPromise = Promise[InetSocketAddress]()
  var clients = Map[InetSocketAddress, (ActorRef, Promise[ActorRef])]()
  val listenerPromise = Promise[ActorRef]()

  def addClient(remote: InetSocketAddress) = {
    val client = context.actorOf(
      ConnectionHandler.props(remote, sender, handler),
      s"c:${remote.getPort}")
    context.watch(client)
    clients += remote -> (client, Promise[ActorRef]())
    sender ! Tcp.Register(client)
    handler ! ("connected", remote)
  }

  def removeClient(client: ActorRef) = {
    clients.find(_._2._1 == client) map { case (remote, (_, promise)) =>
      clients -= remote
      handler ! ("disconnected", remote)
      promise.success(client)
    }
  }

  override def receive: Receive = LoggingReceive {
    case Bound(localAddress) =>
      listener = sender
      context watch listener
      addrPromise.success(localAddress)
    case Unbound =>
      context stop listener
    case Connected(remote, _) =>
      addClient(remote)
    case Terminated(client) =>
      if (client == listener) listenerPromise.success(listener)
      else removeClient(client)
    case (remote: InetSocketAddress, data) =>
      clients.get(remote).map(_._1 ! data)
    // Our stuff
    case "addr" =>
      val asker = sender
      addrPromise.future.onSuccess {
        case addr =>
          asker ! addr
      }
    case "close" =>
      val promises = clients.values.map(_._2) ++ Seq(listenerPromise)
      Future.sequence(promises.map(_.future)) onComplete {
        case _ => context stop self
      }
      listener ! Unbind
      clients.values.foreach(_._1 ! "close")
  }
}

object ConnectionHandler {
  def props(remote: InetSocketAddress, conn: ActorRef, handler: ActorRef): Props =
    Props(new ConnectionHandler(remote, conn, handler))
}

class ConnectionHandler(remote: InetSocketAddress, conn: ActorRef, handler: ActorRef) extends Actor {
  import Tcp._

  context watch conn

  def receive: Receive = LoggingReceive {
    case "close" =>
      conn ! Close
    case Received(data) =>
      handler ! (remote, data)
    case _: ConnectionClosed =>
      context stop conn
    case Terminated(`conn`) =>
      context stop self
    case data: ByteString =>
      conn ! Write(data)
  }
}
