package ircd_conformance

package object constants {
  val RPL_WELCOME  = "001" // Welcome to the Internet Relay Network
                           // <nick>!<user>@<host>
  val RPL_YOURHOST = "002" // Your host is <servername>, running version <ver>
  val RPL_CREATED  = "003" // This server was created <date>
  val RPL_MYINFO   = "004" // <servername> <version> <available user modes>
                           // <available channel modes>
  // - The server sends Replies 001 to 004 to a user upon successful
  // registration.

  val RPL_BOUNCE = "005" // Try server <server name>, port <port number>
  // - Sent by the server to a user to suggest an alternative server. This is
  // often used when the connection is refused because the server is already
  // full.


  // TODO: A whole lot of others.


  val ERR_NONICKNAMEGIVEN = "431" // :No nickname given
  // - Returned when a nickname parameter expected for a command and isn't
  // found.

  val ERR_ERRONEUSNICKNAME = "432" // <nick> :Erroneous nickname
  // - Returned after receiving a NICK message which contains characters which
  // do not fall in the defined set. See section 2.3.1 for details on valid
  // nicknames.

  val ERR_NICKNAMEINUSE = "433" // <nick> :Nickname is already in use
  // - Returned when a NICK message is processed that results in an attempt to
  // change to a currently existing nickname.

  val ERR_NICKCOLLISION = "436" // <nick> :Nickname collision KILL from
                                // <user>@<host>
  // - Returned by a server to a client when it detects a nickname collision
  // (registered of a NICK that already exists by another server).

  val ERR_UNAVAILRESOURCE = "437" // <nick/channel> :Nick/channel is
                                  // temporarily unavailable
  // - Returned by a server to a user trying to join a channel currently
  // blocked by the channel delay mechanism.
  // - Returned by a server to a user trying to change nickname when the
  // desired nickname is blocked by the nick delay mechanism.


  // TODO: Some more.


  val ERR_NEEDMOREPARAMS = "461" // <command> :Not enough parameters
  // - Returned by the server by numerous commands to indicate to the client
  // that it didn't supply enough parameters.


  // TODO: A whole lot of others.

}
