package ircd_conformance.conformance


import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.io.Tcp.Connected
import akka.pattern.ask
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


class TestConformance(_system: ActorSystem) extends TestKit(_system)
    with FreeSpecLike with Matchers with BeforeAndAfterAll {

  def this() =
    this(ActorSystem("TestIrcClient", ConfigFactory.parseMap(Map[String, Any](
      // "akka.loglevel" -> "DEBUG",
      "akka.actor.debug.receive" -> true,
      "akka.log-dead-letters-during-shutdown" -> false))))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }


  var actorcleanup: ActorCleanup = _
  var addr1: InetSocketAddress = _
  var addr2: InetSocketAddress = _
  var logClientMessages: Boolean = _

  override def withFixture(test: NoArgTest) = {
    val (host1, port1) = test.configMap.getRequired[(String, Int)]("addr1")
    addr1 = new InetSocketAddress(host1, port1)
    val (host2, port2) = test.configMap.getRequired[(String, Int)]("addr2")
    addr2 = new InetSocketAddress(host2, port2)
    logClientMessages =
      test.configMap.getOptional[Boolean]("log-client-messages").getOrElse(false)
    actorcleanup = new ActorCleanup(system)
    system.log.debug("\r\n\r\n======= Starting test <{}>", test.name)
    try super.withFixture(test) // Invoke the test function
    finally {
      system.log.debug("\r\n======= Cleaning up test <{}>", test.name)
      actorcleanup.cleanup()
      system.log.debug("\r\n======= Finished test <{}>\r\n", test.name)
    }
  }

  "An IRC server" - {

    case class ActorInfo(ref: ActorRef, probe: TestProbe, addr: InetSocketAddress) {
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

    def commandNumeric(msg: IrcMessage) =
        """^\d\d\d$""".r.findFirstIn(msg.command).isDefined

    implicit def info2ref(ai: ActorInfo): ActorRef = ai.ref
    implicit def info2probe(ai: ActorInfo): TestProbe = ai.probe

    implicit val timeout = Timeout(150 millis)

    def startClient(serverAddr: InetSocketAddress, name: String) = {
      val probe = TestProbe()
      val client = actorcleanup.actorOf(
        IrcClient.props(serverAddr, probe.ref, name, logClientMessages), name)
      val Connected(_, addr) = probe.expectMsgType[Connected]
      ActorInfo(client, probe, addr)
    }

    "during connection setup" - {

      "should complain about missing nick" in {
        val client = startClient(addr1, "client1")
        client ! IrcMessage("NICK")
        val msg = client.getFirstMessageMatching(commandNumeric)
        msg should haveCommandIn(ERR_NONICKNAMEGIVEN, ERR_NEEDMOREPARAMS)
      }

      "should complain about erroneous nick" in {
        val client = startClient(addr1, "client1")
        client ! IrcMessage("NICK", List("+++"))
        val msg = client.getFirstMessageMatching(commandNumeric)
        msg should haveCommandIn(ERR_ERRONEUSNICKNAME)
      }

      "should complain about nick in use" in {
        // Connect with the nick we want to test with.
        val client1 = startClient(addr1, "client1")
        client1 ! IrcMessage("NICK", List("dupnick"))
        client1 ! IrcMessage("USER", List("user", "0", "*", "real name"))
        client1.getFirstMessageMatching(_.command == RPL_WELCOME)
        // Attempt to connect again with same nick.
        val client2 = startClient(addr1, "client2")
        client2 ! IrcMessage("NICK", List("dupnick"))
        val msg = client2.getFirstMessageMatching(commandNumeric)
        msg should haveCommandIn(ERR_NICKNAMEINUSE)
      }

    }
  }
}
