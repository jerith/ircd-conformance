package ircd_conformance.conformance


import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.io.Tcp.Connected
import akka.pattern.ask
import akka.testkit.{TestActor, TestKit, TestProbe}
import akka.util.{ByteString, Timeout}
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scala.collection.JavaConversions._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.{implicitConversions, postfixOps}
import scala.util.matching.Regex

import ircd_conformance.cleanup.ActorCleanup
import ircd_conformance.client.{IrcClient, IrcMessage}
import ircd_conformance.clienthelper.ClientHelper
import ircd_conformance.constants._
import ircd_conformance.matchers.CustomMatchers._


object TestConformance {
  def apply(config: Config) = new TestConformance(config)
}

class TestConformance(config: Config)
    extends TestKit(ActorSystem("TestConformance", config))
    with FreeSpecLike with Matchers with BeforeAndAfterAll
    with GeneratorDrivenPropertyChecks {

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  implicit var actorcleanup: ActorCleanup = _
  val logClientMessages = config.getBoolean("conformance.log-client-messages")
  val timeoutMillis = config.getInt("conformance.timeout")

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

  "An IRC server" - {
    import ircd_conformance.clienthelper.ClientHelperImplicits._
    implicit val timeout = Timeout(timeoutMillis millis)

    def startClient(clientname: String, servername: String) = {
      val serverConfig = config.getConfig(servername)
      val addr = serverConfig.getString("addr")
      ClientHelper.startClient(clientname, addr, logClientMessages)
    }

    "during connection setup" - {

      "should complain about missing nick" in {
        val client = startClient("client1", "ircserver1")
        client ! IrcMessage("NICK")
        val msg = client.getFirstMessageMatching(ClientHelper.commandNumeric)
        msg should haveCommandIn(ERR_NONICKNAMEGIVEN, ERR_NEEDMOREPARAMS)
      }

      "should complain about erroneous nick" in {
        val client = startClient("client1", "ircserver1")
        client ! IrcMessage("NICK", List("+++"))
        val msg = client.getFirstMessageMatching(ClientHelper.commandNumeric)
        msg should haveCommandIn(ERR_ERRONEUSNICKNAME)
      }

      "should complain about nick in use" in {
        // Connect with the nick we want to test with.
        val client1 = startClient("client1", "ircserver1")
        client1 ! IrcMessage("NICK", List("dupnick"))
        client1 ! IrcMessage("USER", List("user", "0", "*", "real name"))
        client1.getFirstMessageMatching(_.command == RPL_WELCOME)
        // Attempt to connect again with same nick.
        val client2 = startClient("client2", "ircserver1")
        client2 ! IrcMessage("NICK", List("dupnick"))
        val msg = client2.getFirstMessageMatching(ClientHelper.commandNumeric)
        msg should haveCommandIn(ERR_NICKNAMEINUSE)
      }

    }
  }
}
