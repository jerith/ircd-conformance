package ircd_conformance.test

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props, Terminated}
import akka.io.Tcp.Connected
import akka.pattern.ask
import akka.testkit.{EventFilter, TestKit, TestProbe, TestEventListener}
import akka.util.{ByteString, Timeout}
import com.typesafe.config.ConfigFactory
import java.net.InetSocketAddress
import org.scalatest._
import scala.collection.JavaConversions._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.{implicitConversions, postfixOps}

import ircd_conformance.cleanup.ActorCleanup
import ircd_conformance.client.{IrcClient, IrcMessage}
import ircd_conformance.test.tags.InternalTest
import ircd_conformance.util.ByteStringHelper

@InternalTest
class TestIrcMessage extends FreeSpec with Matchers {
  val IM = IrcMessage
  def parse(msg: ByteString) = IM.parse(msg)
  def parse(msg: String) = IM.parse(ByteString(msg))

  "An IRC message" - {

    "should build a bytestring" in {
      assert(IM("FOO").bytes === b"FOO")
      assert(IM("prefix", "FOO").bytes === b":prefix FOO")
      assert(IM("FOO", List("bar", "baz")).bytes === b"FOO bar baz")
      assert(IM("pre", "FOO", List("bar")).bytes === b":pre FOO bar")
    }

    "should parse a bytestring" - {

      "with just a command" in {
        assert(parse("FOO") === IM("FOO"))
      }

      "with a prefix and command" in {
        assert(parse(":me FOO") === IM("me", "FOO"))
      }

      "with a simple param" in {
        assert(parse("FOO param") === IM("FOO", List("param")))
      }

      "with simple params" in {
        assert(parse("FOO p1 p2") === IM("FOO", List("p1", "p2")))
      }

      "with a tail param" in {
        assert(parse("FOO :tail param") === IM("FOO", List(":tail param")))
      }

      "with simple and tail params" in {
        assert(parse("FOO p1 :tail p2") === IM("FOO", List("p1", ":tail p2")))
      }

      "with a prefix and simple param" in {
        assert(parse(":me FOO param") === IM("me", "FOO", List("param")))
      }

      "with a prefix and simple params" in {
        assert(parse(":me FOO p1 p2") === IM("me", "FOO", List("p1", "p2")))
      }

      "with a prefix and tail param" in {
        assert(parse(":me FOO :tail param") ===
          IM("me", "FOO", List(":tail param")))
      }

      "with a prefix and simple and tail params" in {
        assert(parse(":me FOO p1 :tail p2") ===
          IM("me", "FOO", List("p1", ":tail p2")))
      }

      "with a non-ASCII prefix" in {
        assert(parse(b":me\xc3\x28 FOO p1 :tail p2") ===
          IM("me\u00c3\u0028", "FOO", List("p1", ":tail p2")))
      }

      "with a non-ASCII command" in {
        assert(parse(b":me FOO\xc3\x28 p1 :tail p2") ===
          IM("me", "FOO\u00c3\u0028", List("p1", ":tail p2")))
      }

      "with a non-ASCII params" in {
        assert(parse(b":me FOO p1\xc3\x28 :tail p2") ===
          IM("me", "FOO", List("p1\u00c3\u0028", ":tail p2")))
        assert(parse(b":me FOO p1 :tail p2\xc3\x28") ===
          IM("me", "FOO", List("p1", ":tail p2\u00c3\u0028")))
      }

    }
  }
}

@InternalTest
class TestIrcClient(_system: ActorSystem) extends TestKit(_system)
    with FreeSpecLike with Matchers with BeforeAndAfterAll {

  def this() =
    this(ActorSystem("TestIrcClient", ConfigFactory.parseMap(Map[String, Any](
      "akka.loggers" -> seqAsJavaList(Seq("akka.testkit.TestEventListener")),
      // "akka.loglevel" -> "DEBUG",
      "akka.actor.debug.receive" -> true,
      "akka.log-dead-letters-during-shutdown" -> false))))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  var actorcleanup: ActorCleanup = _

  override def withFixture(test: NoArgTest) = {
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

    case class ActorInfo(ref: ActorRef, probe: TestProbe, addr: InetSocketAddress) {
      // For some reason, ! doesn't get proxied.
      val ! = ref.!(_)
    }

    implicit def info2ref(ai: ActorInfo): ActorRef = ai.ref
    implicit def info2probe(ai: ActorInfo): TestProbe = ai.probe

    implicit val timeout = Timeout(150 millis)

    def startServer() = {
      val probe = TestProbe()
      val listenAddr = new InetSocketAddress("localhost", 0)
      val server = actorcleanup.actorOf(
        FakeServer.props(listenAddr, probe.ref), "server")
      val addrFuture = server.ask("addr").mapTo[InetSocketAddress]
      val addr = Await.result(addrFuture, timeout.duration)
      ActorInfo(server, probe, addr)
    }

    def startClient(server: ActorInfo, name: String = "client", log: Boolean = false) = {
      val probe = TestProbe()
      val client = actorcleanup.actorOf(
        IrcClient.props(server.addr, probe.ref, name, log), name)
      val Connected(_, addr) = probe.expectMsgType[Connected]
      server.probe.expectMsg(("connected", addr))
      ActorInfo(client, probe, addr)
    }

    "should connect and disconnect" in {
      val server = startServer()
      val client = startClient(server)
      // startClient only returns after a connection is made.
      client ! "close"
      server.expectMsg(("disconnected", client.addr))
      client.expectMsg("disconnected")
      actorcleanup.awaitTerminated(client)
    }

    "should send ByteStrings" in {
      val server = startServer()
      val client = startClient(server)
      client ! ByteString("Hello")
      server.expectMsg((client.addr, ByteString("Hello")))
    }

    "should send IrcMessages" in {
      val server = startServer()
      val client = startClient(server)
      client ! IrcMessage("LIST")
      server.expectMsg((client.addr, ByteString("LIST\r\n")))
    }

    "should receive IrcMessages" in {
      val server = startServer()
      val client = startClient(server)
      server ! (client.addr, ByteString("PING :server\r\n"))
      client.expectMsg(IrcMessage("PING", List(":server")))
    }

    "should log when configured to do so" in {
      val server = startServer()
      val client = startClient(server, name = "clientlog", log = true)
      val inpattern = raw"<<clientlog\|\| PING :server"
      EventFilter.info(pattern = inpattern, occurrences = 1) intercept {
        server ! (client.addr, ByteString("PING :server\r\n"))
        client.expectMsg(IrcMessage("PING", List(":server")))
      }
      val outpattern = raw"\|\|clientlog>> PING :server2"
      EventFilter.info(pattern = outpattern, occurrences = 1) intercept {
        client ! IrcMessage("PING", List(":server2"))
        server.expectMsg((client.addr, ByteString("PING :server2\r\n")))
      }
    }

    "should not log when not configured to do so" in {
      val server = startServer()
      val client = startClient(server, name = "clientnolog", log = false)
      val inpattern = raw"<<clientnolog\|\| PING :server"
      EventFilter.info(pattern = inpattern, occurrences = 0) intercept {
        server ! (client.addr, ByteString("PING :server\r\n"))
        client.expectMsg(IrcMessage("PING", List(":server")))
      }
      val outpattern = raw"\|\|clientnolog>> PING :server2"
      EventFilter.info(pattern = outpattern, occurrences = 0) intercept {
        client ! IrcMessage("PING", List(":server2"))
        server.expectMsg((client.addr, ByteString("PING :server2\r\n")))
      }
    }
  }
}
