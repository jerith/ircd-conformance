package ircd_conformance.test

import scala.language.postfixOps
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{TestKit, TestProbe}
import akka.util.{ByteString, Timeout}
import akka.io.Tcp.Connected
import ircd_conformance.client.{IrcClient, IrcMessage}
import ircd_conformance.test.tags.InternalTest
import java.net.InetSocketAddress
import org.scalatest._
import scala.concurrent.Await
import akka.pattern.ask
import scala.concurrent.duration._
import com.typesafe.config.ConfigFactory
import collection.JavaConversions._
import akka.actor.PoisonPill

@InternalTest
class TestIrcMessage extends FreeSpec with Matchers {
  val IM = IrcMessage
  val BS = ByteString
  def parse(msg: String) = IM.parse(BS(msg))

  "An IRC message" - {

    "should build a bytestring" in {
      assert(IM("FOO").bytes === BS("FOO"))
      assert(IM("prefix", "FOO").bytes === BS(":prefix FOO"))
      assert(IM("FOO", List("bar", "baz")).bytes === BS("FOO bar baz"))
      assert(IM("pre", "FOO", List("bar")).bytes === BS(":pre FOO bar"))
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

    }
  }
}


@InternalTest
class TestIrcClient(_system: ActorSystem) extends TestKit(_system)
    with FreeSpecLike with Matchers with BeforeAndAfterAll {

  def this() =
    this(ActorSystem("TestIrcClient", ConfigFactory.parseMap(Map[String, Any](
      // "akka.loglevel" -> "DEBUG",
      "akka.actor.debug.receive" -> true,
      "akka.log-dead-letters-during-shutdown" -> false))))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  var actors: Seq[ActorRef] = Nil

  override def withFixture(test: NoArgTest) = {
    actors = Nil
    _system.log.debug("Starting test <{}>", test.name)
    try super.withFixture(test) // Invoke the test function
    finally {
      _system.log.debug("Cleaning up test <{}>", test.name)
      val deathwatch = TestProbe()
      actors.reverse.foreach { actor =>
        deathwatch.watch(actor)
        actor ! PoisonPill
        deathwatch.expectTerminated(actor, 100 millis)
      }
      _system.log.debug("Finished test <{}>", test.name)
    }
  }

  "An IRC client" - {

    case class ActorInfo(val ref: ActorRef, val probe: TestProbe, val addr: InetSocketAddress)

    implicit val timeout = Timeout(150 millis)

    def startServer() = {
      val probe = TestProbe()
      val listenAddr = new InetSocketAddress("localhost", 0)
      val server = system.actorOf(
        FakeServer.props(listenAddr, probe.ref), "server")
      actors :+= server
      val addrFuture = server.ask("addr").mapTo[InetSocketAddress]
      val addr = Await.result(addrFuture, timeout.duration)
      ActorInfo(server, probe, addr)
    }

    def startClient(server: ActorInfo, name: String = "client") = {
      val probe = TestProbe()
      val client = system.actorOf(IrcClient.props(server.addr, probe.ref), name)
      actors :+= client
      val Connected(_, addr) = probe.expectMsgType[Connected]
      server.probe.expectMsg(("connected", addr))
      ActorInfo(client, probe, addr)
    }

    "should connect and disconnect" in {
      val server = startServer()
      val client = startClient(server)
      // startClient only returns after a connection is made.
      client.ref ! "close"
      server.probe.expectMsg(("disconnected", client.addr))
      client.probe.expectMsg("disconnected")
    }

    "should send ByteStrings" in {
      val server = startServer()
      val client = startClient(server)
      client.ref ! ByteString("Hello")
      server.probe.expectMsg((client.addr, ByteString("Hello")))
    }

    "should send IrcMessages" in {
      val server = startServer()
      val client = startClient(server)
      client.ref ! IrcMessage("LIST")
      server.probe.expectMsg((client.addr, ByteString("LIST\r\n")))
    }

    "should receive IrcMessages" in {
      val server = startServer()
      val client = startClient(server)
      server.ref ! (client.addr, ByteString("PING :server\r\n"))
      client.probe.expectMsg(IrcMessage("PING", List(":server")))
    }

  }
}
