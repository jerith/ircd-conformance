package ircd_conformance.conformance

import com.typesafe.config.ConfigFactory
import org.scalatest.ConfigMap


object Main extends App {
  TestConformance(ConfigFactory.load()).execute()
}
