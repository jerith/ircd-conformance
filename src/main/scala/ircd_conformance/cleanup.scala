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
      val probe = TestProbe()(system)
      probe.watch(deathwatcher)

      def shutdown() = {
        actor ! "close"
        awaitTerminated()
      }

      def awaitTerminated() = {
        probe.expectTerminated(deathwatcher, 100 millis)
      }
    }

    def watchActor(actor: ActorRef) = {
      deathwatches :+= DeathWatch(actor)
      actor
    }

    def cleanup() = deathwatches.reverse.foreach(_.shutdown())

    def awaitTerminated(actor: ActorRef) =
      deathwatches.find(_.actor == actor).map(_.awaitTerminated())

    def actorOf(props: Props, name: String) =
      watchActor(system.actorOf(props, name))
  }
}
