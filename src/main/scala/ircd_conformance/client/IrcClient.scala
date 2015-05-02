package ircd_conformance.client

import akka.actor.{ Actor, ActorLogging, ActorRef, ActorSystem, Props, Terminated }
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import java.net.InetSocketAddress
import akka.event.LoggingReceive
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader

case class IrcMessage(prefix: String, command: String, params: List[String]) {
  private val prefixString = if (prefix == "") "" else ":" + prefix + " "
  val bytes = ByteString(prefixString + (command :: params).mkString(" "))
}

object IrcMessage {
  def apply(prefix: String, command: String) =
    new IrcMessage(prefix, command, Nil)

  def apply(command: String, params: List[String] = Nil) =
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

    def parse(bytes: ByteString) =
      phrase(message)(new CharSequenceReader(bytes.utf8String)) match {
        case Success(message, _) => message
        case NoSuccess(_, _) => sys.error("oops")
      }
  }

  def parse(bytes: ByteString) = IrcMessageParser.parse(bytes)
}


object IrcClient {
  def props(remote: InetSocketAddress, replies: ActorRef) =
    Props(classOf[IrcClient], remote, replies)
}

class IrcClient(remote: InetSocketAddress, handler: ActorRef) extends Actor {
  import Tcp._
  import context.system

  IO(Tcp) ! Connect(remote)

  var dataBuffer = ByteString()

  def receiveData(data: ByteString): Unit = this.synchronized {
    dataBuffer ++= data
    val nextNewline = dataBuffer.indexOfSlice(ByteString("\r\n"))
    if (nextNewline >= 0) {
      val line = dataBuffer.take(nextNewline)
      dataBuffer = dataBuffer.drop(nextNewline + 2)
      handler ! IrcMessage.parse(line)
      receiveData(ByteString())
    }
  }

  def receive_connected(conn: ActorRef): Receive = LoggingReceive {
    // TCP things
    case CommandFailed(w: Write) =>
      // O/S buffer was full
      handler ! "write failed"
    case Received(data) =>
      receiveData(data)
    case _: ConnectionClosed =>
      handler ! "disconnected"
      context stop self

    // Other things
    case data: ByteString =>
      conn ! Write(data)
    case msg: IrcMessage =>
      conn ! Write(msg.bytes ++ ByteString("\r\n"))
    case "close" =>
      conn ! Close
  }

  def receive = LoggingReceive {
    case CommandFailed(_: Connect) =>
      handler ! "connect failed"
      context stop self

    case c @ Connected(remote, local) =>
      handler ! c
      val connection = sender()
      connection ! Register(self)
      context become receive_connected(connection)
  }
}
