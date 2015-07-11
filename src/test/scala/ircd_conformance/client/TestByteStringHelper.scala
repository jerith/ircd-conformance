package ircd_conformance.test

import akka.pattern.ask
import akka.util.ByteString
import ircd_conformance.test.tags.InternalTest
import ircd_conformance.util.ByteStringHelper
import org.scalatest._
import scala.collection.JavaConversions._
import scala.language.{implicitConversions, postfixOps}

@InternalTest
class TestByteStringHelper extends FreeSpec with Matchers {
  "A bytestring processor" - {
    "should build a bytestring" in {
      assert(b"hello" === ByteString("hello"))
    }

    "should accept bytestring args" in {
      val name = ByteString("you")
      assert(b"hello $name" === ByteString("hello you"))
    }

    "should accept hexadecimal escapes" in {
      assert(b"a\xc3b\x28c" === ByteString('a', 0xc3, 'b', 0x28, 'c'))
    }

    "should accept octal escapes" in {
      assert(b"\0" === ByteString(0))
      assert(b"\00" === ByteString(0))
      assert(b"\000" === ByteString(0))
      assert(b"\0000" === ByteString(0, '0'))
      assert(b"\3" === ByteString(3))
      assert(b"\37" === ByteString(31))
      assert(b"\377" === ByteString(255))
      assert(b"\3777" === ByteString(255, '7'))
      assert(b"\4" === ByteString(4))
      assert(b"\40" === ByteString(32))
      assert(b"\400" === ByteString(32, '0'))
      assert(b"\4000" === ByteString(32, '0', '0'))
    }

    "should accept octal escapes followed by 9" in {
      assert(b"\09" === ByteString(0, '9'))
      assert(b"\009" === ByteString(0, '9'))
      assert(b"\0009" === ByteString(0, '9'))
    }

    // TODO: More tests, especially failure cases.
  }
}
