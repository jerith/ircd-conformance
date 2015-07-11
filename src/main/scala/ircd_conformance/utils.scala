package ircd_conformance

import akka.util.{ByteString, ByteStringBuilder}
import scala.StringContext.InvalidEscapeException

package object util {
  implicit class ByteStringHelper(val sc: StringContext) extends AnyVal {
    def b(args: Any*): ByteString = {
      sc.checkLengths(args)
      val builder = ByteString.newBuilder
      build(sc.parts.head, builder)
      args.zip(sc.parts.tail).foreach { pieces =>
        pieces._1 match {
          case arg: String => build(arg, builder)
          case arg: ByteString => build(arg.decodeString("ISO-8859-1"), builder)
          case _ => throw new IllegalArgumentException(
            "args must be ByteStrings or ISO-8859-1 encoded Strings")
        }
        build(pieces._2, builder)
      }
      builder.result()
    }

    private def isOct(char: Char) = '0' <= char && char <= '7'

    private def build(input: String, builder: ByteStringBuilder): Unit = {
      val len = input.length
      var start = 0
      while (start < len) {
        input(start) match {
          case char @ '\\' =>
            if (start + 1 >= len)
              throw new InvalidEscapeException(input, start + 1)
            var consume = 2
            val byte: Byte = input(start + 1) match {
              case 'b'  => '\b'
              case 't'  => '\t'
              case 'n'  => '\n'
              case 'f'  => '\f'
              case 'r'  => '\r'
              case '"'  => '"'
              case '\'' => '\''
              case '\\' => '\\'

              case 'x' =>
                consume += 2
                if (start + 3 >= len)
                  throw new InvalidEscapeException(input, start + 1)
                Integer.parseInt(input.slice(start + 2, start + 4), 16).toByte

              case o if isOct(o) =>
                val end = if (o > '3') 3 else 4
                val digits = input.slice(start + 1, end).takeWhile(isOct)
                if (digits.isEmpty)
                  throw new InvalidEscapeException(input, start + 1)
                consume = 1 + digits.length
                Integer.parseInt(digits, 8).toByte

              case _ =>
                throw new InvalidEscapeException(input, start + 1)
            }
            builder.putByte(byte)
            start += consume
          case char =>
            builder.putByte(char.toByte)
            start += 1
        }
      }
    }
  }
}



//             build(input.drop(2), builder)
//       case "" => ()
//       case _ if input.startsWith("\\") => {
//         if (input.length < 2)
//           throw new InvalidEscapeException(input, -1)

//         input(1) match {
//           case '\\' | '"' | '\'' =>
//             builder.putByte(input(1).toByte)
//             build(input.drop(2), builder)
//           case 'n' =>
//             builder.putByte('\n')
//             build(input.drop(2), builder)
//           case _ =>
//         }
//       }
//       case _ =>
//         builder.putByte(input.head.toByte)
//         build(input.tail, builder)
//     }
//   }
// }
