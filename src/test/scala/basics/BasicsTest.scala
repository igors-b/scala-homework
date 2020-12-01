package basics

import Basics._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class BasicsTest extends  AnyFlatSpec {

  "gcd" should "calculate Greatest common divisor for two integers" in {
    gcd(18,84) shouldEqual 6
    gcd(180,48) shouldEqual 12
    gcd(-34, 4) shouldEqual 2
  }

  "lcm" should "calculate Lowest common denominator for two integers" in {
    lcm(21, 6) shouldEqual 42
    lcm(-13, 4) shouldEqual 52
  }
}
