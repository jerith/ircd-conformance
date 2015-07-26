package ircd_conformance.generators

import org.scalacheck.Gen


object GeneratorSources {
  def chars(nums: Seq[Int]) = nums.map(_.toChar)

  val letter = chars((0x41 to 0x5A) ++ (0x61 to 0x7A))
  val digit = chars(0x30 to 0x39)
  val special = "[]\\`_^{||".toSeq
}


object Generators {
  import GeneratorSources._

  val nickname: Gen[String] = for {
    first <- Gen.oneOf(letter ++ special)
    rest <- Gen.choose(0, 8) flatMap { len =>
      Gen.listOfN(len, Gen.oneOf(letter ++ digit ++ special :+ '-'))
    }
  } yield (first +: rest).mkString

  val transformCase = Gen.oneOf(
    (s: String) => s.toUpperCase(),
    (s: String) => s.toLowerCase(),
    (s: String) => s)
}
