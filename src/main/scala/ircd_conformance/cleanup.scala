package ircd_conformance

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Terminated}
import akka.testkit.TestProbe
import scala.concurrent.duration._
import scala.language.postfixOps

package object cleanup {

  class ActorCleanup(system: ActorSystem) {

    var deathwatches: Seq[DeathWatch] = Nil

    class DeathWatcher(actor: ActorRef, deathwatch: DeathWatch) extends Actor {
      context watch actor

      def receive = {
        case Terminated(`actor`) =>
          deathwatches = deathwatches.filter(_ != deathwatch)
          context stop self
      }
    }

    case class DeathWatch(actor: ActorRef) {
      val deathwatcher = system.actorOf(Props(new DeathWatcher(actor, this)))

      def shutdown() = {
        val probe = TestProbe()(system)
        probe.watch(deathwatcher)
        actor ! "close"
        probe.expectTerminated(deathwatcher, 100 millis)
      }
    }

    def watchActor(actor: ActorRef) = deathwatches :+= DeathWatch(actor)

    def cleanup() =
      deathwatches.reverse.foreach { deathwatch => deathwatch.shutdown() }

    def actorOf(props: Props, name: String) = {
      val actor = system.actorOf(props, name)
      watchActor(actor)
      actor
    }
  }
}
