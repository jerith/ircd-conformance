package ircd_conformance.conformance

import org.scalatest.ConfigMap


object Main extends App {
  (new TestConformance).execute(configMap = ConfigMap(
    "addr1" -> ("localhost", 12345),
    "addr2" -> ("localhost", 12346)
  ))
}
