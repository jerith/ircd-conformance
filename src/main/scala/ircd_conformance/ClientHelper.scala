package ircd_conformance.clienthelper


import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.io.Tcp.Connected
import akka.testkit.{TestActor, TestKit, TestProbe}
import akka.util.{ByteString, Timeout}
import com.typesafe.config.ConfigFactory
import java.net.InetSocketAddress
import org.scalatest._
import scala.collection.JavaConversions._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.{implicitConversions, postfixOps}
import scala.util.matching.Regex

import ircd_conformance.cleanup.ActorCleanup
import ircd_conformance.client.{IrcClient, IrcMessage}
import ircd_conformance.constants._
import ircd_conformance.matchers.CustomMatchers._


case class ClientHelper(ref: ActorRef, probe: TestProbe, serveraddr: InetSocketAddress, name: String)(implicit actorcleanup: ActorCleanup, system: ActorSystem) {
  // For some reason, ! doesn't get proxied.
  val ! = ref.!(_)

  // For convenience.
  def disconnect() = {
    ref ! "close"
    swallowIrcMessages()
    probe.expectMsg("disconnected")
    actorcleanup.awaitTerminated(ref)
  }

  def swallowIrcMessages(p: Function[IrcMessage, Boolean] = (_ => true)) =
    probe.receiveWhile() { case msg: IrcMessage if p(msg) => }

  def getFirstMessageMatching(p: Function[IrcMessage, Boolean]) = {
    swallowIrcMessages(!p(_))
    probe.receiveOne(500 millis).asInstanceOf[IrcMessage]
  }
}


object ClientHelperImplicits {
  implicit def info2ref(ai: ClientHelper): ActorRef = ai.ref
  implicit def info2probe(ai: ClientHelper): TestProbe = ai.probe
}

object ClientHelper {
  def commandNumeric(msg: IrcMessage) =
    """^\d\d\d$""".r.findFirstIn(msg.command).isDefined

  def startClient(name: String, serveraddr: String, logClientMessages: Boolean)(implicit actorcleanup: ActorCleanup, system: ActorSystem) = {
    val probe = TestProbe()
    val client = actorcleanup.actorOf(
      IrcClient.props(serveraddr, probe.ref, name, logClientMessages), name)
    val Connected(_, addr) = probe.expectMsgType[Connected]
    ClientHelper(client, probe, addr, name)
  }
}
