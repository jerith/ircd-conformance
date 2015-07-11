package ircd_conformance.conformance


import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.io.Tcp.Connected
import akka.pattern.ask
import akka.testkit.{TestKit, TestProbe}
import akka.util.{ByteString, Timeout}
import com.typesafe.config.ConfigFactory
import java.net.InetSocketAddress
import org.scalatest._
import scala.collection.JavaConversions._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

import ircd_conformance.cleanup.ActorCleanup
import ircd_conformance.client.{IrcClient, IrcMessage}


class TestConformance(_system: ActorSystem) extends TestKit(_system)
    with FreeSpecLike with Matchers with BeforeAndAfterAll {

  def this() =
    this(ActorSystem("TestIrcClient", ConfigFactory.parseMap(Map[String, Any](
      "akka.loglevel" -> "DEBUG",
      "akka.actor.debug.receive" -> true,
      "akka.log-dead-letters-during-shutdown" -> false))))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }


  var actorcleanup: ActorCleanup = _
  var addr1: InetSocketAddress = _
  var addr2: InetSocketAddress = _

  override def withFixture(test: NoArgTest) = {
    val (host1, port1) = test.configMap.getRequired[(String, Int)]("addr1")
    addr1 = new InetSocketAddress(host1, port1)
    val (host2, port2) = test.configMap.getRequired[(String, Int)]("addr2")
    addr2 = new InetSocketAddress(host2, port2)
    actorcleanup = new ActorCleanup(system)
    system.log.debug("\r\n\r\n======= Starting test <{}>", test.name)
    try super.withFixture(test) // Invoke the test function
    finally {
      system.log.debug("\r\n======= Cleaning up test <{}>", test.name)
      actorcleanup.cleanup()
      system.log.debug("\r\n======= Finished test <{}>\r\n", test.name)
    }
  }

  "An IRC client" - {

    case class ActorInfo(val ref: ActorRef, val probe: TestProbe, val addr: InetSocketAddress)

    implicit val timeout = Timeout(150 millis)

    def startClient(serverAddr: InetSocketAddress, name: String) = {
      val probe = TestProbe()
      val client = actorcleanup.actorOf(
        IrcClient.props(serverAddr, probe.ref), name)
      val Connected(_, addr) = probe.expectMsgType[Connected]
      ActorInfo(client, probe, addr)
    }

    "should connect and disconnect" in {
      val client = startClient(addr1, "client1")
      client.probe.receiveWhile(3 seconds, 500 millis, 20) {
        case msg => println(s"RECEIVED -> $msg")
      }
      client.ref ! IrcMessage("NICK c1")
      client.ref ! "close"
      client.probe.expectMsg("disconnected")
    }

  }
}
