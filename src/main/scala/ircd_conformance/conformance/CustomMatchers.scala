package ircd_conformance.matchers

import org.scalatest._
import matchers._

import ircd_conformance.client.IrcMessage

trait CustomMatchers {

  class MessageCommandMatcher(expectedCommands: Seq[String]) extends Matcher[IrcMessage] {

    def apply(msg: IrcMessage) = MatchResult(
      expectedCommands.contains(msg.command),
      s"""Message $msg does not have a command in $expectedCommands""",
      s"""Message $msg has a command in $expectedCommands""")
  }

  def haveCommandIn(expectedCommands: String*) =
    new MessageCommandMatcher(expectedCommands)
}

// Make them easy to import with:
// import CustomMatchers._
object CustomMatchers extends CustomMatchers
