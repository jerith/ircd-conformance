package ircd_conformance.addresshelper

import java.net.InetSocketAddress
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader


object AddressHelper {

  private object InetAddressParser extends Parsers {
    import scala.language.postfixOps

    type Elem = Char

    def hostname: Parser[String] = (elem("ch", _ != ':')*) ^^ (_.mkString)
    def port: Parser[String] = (elem("ch", _.isDigit)*) ^^ (_.mkString)
    def addr: Parser[InetSocketAddress] = hostname ~ elem(':') ~ port ^^ {
      case hostname ~ _ ~ port =>
        new InetSocketAddress(hostname, Integer.parseInt(port))
    }

    def parse(address: String): InetSocketAddress =
      addr(new CharSequenceReader(address)) match {
        case Success(address, _) => address
        case NoSuccess(_, _) => sys.error("oops")
      }
  }

  def parseAddress(address: String) = InetAddressParser.parse(address)

  def addressToString(address: InetSocketAddress) =
    s"${address.getHostString()}:${address.getPort()}"
}
