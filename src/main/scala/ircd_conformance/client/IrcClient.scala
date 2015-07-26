package ircd_conformance.client

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props, Terminated}
import akka.event.LoggingReceive
import akka.io.{IO, Tcp}
import akka.util.ByteString
import java.net.InetSocketAddress
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader

import ircd_conformance.addresshelper.AddressHelper.parseAddress


/**
  * Internally, we work with ISO-8859-1 strings, rather than bytestrings.
  */

object bsutils {
  def b2s(bytes: ByteString): String = bytes.decodeString("ISO-8859-1")
  def s2b(str: String): ByteString = ByteString(str, "ISO-8859-1")
  val bcrlf = s2b("\r\n")
}


case class IrcMessage(prefix: String, command: String, params: List[String]) {
  private val prefixString = if (prefix == "") "" else ":" + prefix + " "
  val bytes = bsutils.s2b(prefixString + (command :: params).mkString(" "))
  override def toString(): String = bsutils.b2s(bytes)
}

object IrcMessage {
  def apply(prefix: String, command: String): IrcMessage =
    new IrcMessage(prefix, command, Nil)

  def apply(command: String, params: List[String] = Nil): IrcMessage =
    new IrcMessage("", command, params)

  object IrcMessageParser extends Parsers {
    import scala.language.postfixOps

    type Elem = Char

    def space: Parser[Unit] = elem(' ') ^^^ ()
    def ch: Parser[Elem] = elem("ch", _ != ' ')
    def anych: Parser[Elem] = elem("anych", _ => true)
    def word: Parser[String] = (ch*) ^^ (_.mkString)

    def prefix: Parser[String] = elem(':') ~> word <~ space

    def command: Parser[String] = word

    def paramword: Parser[String] = space ~> not(elem(':')) ~> word
    def paramtail: Parser[String] =
      space ~> elem(':') ~> (anych*) ^^ { chars => ":" + chars.mkString }
    def params: Parser[List[String]] = (paramword*) ~ (paramtail?) ^^ {
      case params ~ None => params
      case params ~ Some(tail) => params :+ tail
    }

    def message: Parser[IrcMessage] = (prefix?) ~ command ~ params ^^ {
      case prefix ~ command ~ params =>
        new IrcMessage(prefix.getOrElse(""), command, params)
    }

    def parse(bytes: ByteString): IrcMessage =
      phrase(message)(new CharSequenceReader(bsutils.b2s(bytes))) match {
        case Success(message, _) => message
        case NoSuccess(_, _) => sys.error("oops")
      }
  }

  def parse(bytes: ByteString): IrcMessage = IrcMessageParser.parse(bytes)
}


object IrcClient {
  def props(remoteAddr: String, replies: ActorRef, name: String, logMessages: Boolean) =
    Props(classOf[IrcClient], remoteAddr, replies, name, logMessages)
}


class IrcClient(remoteAddr: String, handler: ActorRef, name: String, logMessages: Boolean) extends Actor {
  import Tcp._
  import context.system

  IO(Tcp) ! Connect(parseAddress(remoteAddr))

  var dataBuffer = ByteString()

  def receiveData(data: ByteString): Unit = this.synchronized {
    dataBuffer ++= data
    val nextNewline = dataBuffer.indexOfSlice(bsutils.bcrlf)
    if (nextNewline >= 0) {
      val line = dataBuffer.take(nextNewline)
      dataBuffer = dataBuffer.drop(nextNewline + 2)
      val msg = IrcMessage.parse(line)
      logIrcMessage(msg, s"<<$name||")
      handler ! msg
      receiveData(ByteString())
    }
  }

  def logIrcMessage(msg: IrcMessage, prefix: String) =
    if (logMessages) system.log.info("\r\n {} {}", prefix, msg)

  def receive_connected(conn: ActorRef): Receive = LoggingReceive {
    // TCP things
    case CommandFailed(w: Write) =>
      handler ! "write failed"
    case Received(data) =>
      receiveData(data)
    case _: ConnectionClosed =>
      handler ! "disconnected"
      context stop conn

    // Other things
    case Terminated(`conn`) =>
      context stop self
    case data: ByteString =>
      conn ! Write(data)
    case msg: IrcMessage =>
      logIrcMessage(msg, s"||$name>>")
      conn ! Write(msg.bytes ++ bsutils.bcrlf)
    case "close" =>
      conn ! Close
  }

  def receive: Receive = LoggingReceive {
    case CommandFailed(_: Connect) =>
      handler ! "connect failed"
      context stop self

    case c @ Connected(remote, local) =>
      handler ! c
      val connection = sender()
      connection ! Register(self)
      context watch connection
      context become receive_connected(connection)
  }
}
